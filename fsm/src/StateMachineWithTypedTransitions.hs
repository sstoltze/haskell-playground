-- https://wickstrom.tech/finite-state-machines/2017/11/19/finite-state-machines-part-2.html
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module StateMachineWithTypedTransitions where

import           Control.Monad.IO.Class
import           Data.List.NonEmpty
import           Data.Semigroup
import qualified Data.Text as T

import           Checkout (Card(..)
                , CartItem
                , OrderId(..)
                , mkItem
                , calculatePrice
                , newOrderId
                )
import qualified ConsoleInput
import qualified PaymentProvider

-- Empty datatypes represent states
data NoItems
data HasItems
data NoCard
data CardSelected
data CardConfirmed
data OrderPlaced

-- FSM protocol - purely in types
class Checkout m where
  type State m :: * -> *
  initial :: m (State m NoItems)
  select :: SelectState m -> CartItem -> m (State m HasItems)
  checkout :: State m HasItems -> m (State m NoCard)
  selectCard :: State m NoCard -> Card -> m (State m CardSelected)
  confirm :: State m CardSelected -> m (State m CardConfirmed)
  placeOrder :: State m CardConfirmed -> m (State m OrderPlaced)
  cancel :: CancelState m -> m (State m HasItems)
  end :: State m OrderPlaced -> m OrderId

data SelectState m = NoItemsSelect (State m NoItems)
                   | HasItemsSelect (State m HasItems)

data CancelState m = NoCardCancel (State m NoCard)
                   | CardSelectedCancel (State m CardSelected)
                   | CardConfirmedCancel (State m CardConfirmed)

-- Implementation
fillCart :: (Checkout m, MonadIO m) => State m NoItems -> m (State m HasItems)
fillCart noItems = mkItem <$> ConsoleInput.prompt "First item:"
  >>= select (NoItemsSelect noItems)
  >>= selectMoreItems

selectMoreItems :: (Checkout m, MonadIO m) => State m HasItems -> m (State m HasItems)
selectMoreItems s = do
  more <- ConsoleInput.confirm "More items?"
  if more
    then
      mkItem <$> ConsoleInput.prompt "Next item:"
      >>= select (HasItemsSelect s)
      >>= selectMoreItems
    else return s

startCheckout :: (Checkout m, MonadIO m) => State m HasItems -> m (State m OrderPlaced)
startCheckout hasItems = do
  noCard <- checkout hasItems
  card <- ConsoleInput.prompt "Card:"
  cardSelected <- selectCard noCard (Card card)
  useCard <- ConsoleInput.confirm ("Confirm use of '" <> card <>"'?")
  if useCard
    then confirm cardSelected >>= placeOrder
    else cancel (CardSelectedCancel cardSelected) >>= selectMoreItems >>= startCheckout

checkoutProgram :: (Checkout m, MonadIO m) => m OrderId
checkoutProgram = initial >>= fillCart >>= startCheckout >>= end

-- To use the program, we need a Checkout instance
newtype CheckoutT m a = CheckoutT { runCheckoutT :: m a }
  deriving ( Functor
           , Monad
           , Applicative
           , MonadIO
           )

data CheckoutState s where
  NoItems :: CheckoutState NoItems
  HasItems :: NonEmpty CartItem -> CheckoutState HasItems
  NoCard :: NonEmpty CartItem -> CheckoutState NoCard
  CardSelected :: NonEmpty CartItem -> Card -> CheckoutState CardSelected
  CardConfirmed :: NonEmpty CartItem -> Card -> CheckoutState CardConfirmed
  OrderPlaced :: OrderId -> CheckoutState OrderPlaced

instance (MonadIO m) => Checkout (CheckoutT m) where
  type State (CheckoutT m) = CheckoutState
  initial = return NoItems
  select state item =
    case state of
      NoItemsSelect NoItems -> return (HasItems (item :| []))
      HasItemsSelect (HasItems items) -> return (HasItems (item <| items))
  checkout (HasItems items) = return (NoCard items)
  selectCard (NoCard items) card = return (CardSelected items card)
  confirm (CardSelected items card) = return (CardConfirmed items card)
  placeOrder (CardConfirmed items card) = do
    orderId <- newOrderId
    let price = calculatePrice items
    PaymentProvider.chargeCard card price
    return (OrderPlaced orderId)
  cancel cancelState =
    case cancelState of
      NoCardCancel (NoCard items) -> return (HasItems items)
      CardSelectedCancel (CardSelected items _) -> return (HasItems items)
      CardConfirmedCancel (CardConfirmed items _) -> return (HasItems items)
  end (OrderPlaced orderId) = return orderId

example :: IO ()
example = do
  OrderId orderId <- runCheckoutT checkoutProgram
  putStrLn $ T.unpack ("Completed with order ID: " <> orderId)
