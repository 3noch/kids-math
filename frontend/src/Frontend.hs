{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Frontend where

import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Text.Read (readMaybe)
import Data.Foldable (traverse_)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)

import Common.Api
import Common.Route
import Obelisk.Generated.Static


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "Math Practice for Aiden"
  , _frontend_body = do
      let maxDim = 8
      let step = Workflow $ do
                q <- liftIO $ do
                  which <- randomRIO (False, True)
                  let
                    ctor = case which of
                      False -> Question_Area
                      True -> Question_Perimeter
                  shouldShowArray <- randomRIO (False, True)
                  showArray <- case shouldShowArray of
                    False -> pure Nothing
                    True -> Just . ShowArray <$> randomRIO (30,60)

                  ctor showArray <$> (randomRIO (1, maxDim)) <*> (randomRIO (1, maxDim))
                next <- questionWidget q
                pure ((), step <$ next)
      _ <- workflow step
      pure ()
  }

newtype ShowArray = ShowArray Int deriving (Eq, Ord, Enum, Bounded, Show)

data Question
  = Question_Area (Maybe ShowArray) Int Int
  | Question_Perimeter (Maybe ShowArray) Int Int
  deriving (Eq, Ord, Show)

questionWidget :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m) => Question -> m (Event t ())
questionWidget q = do
  (preambleCorrect, rightAnswer) <- case q of
    Question_Area showingArray w h -> do
      preambleCorrect <- preamble w h showingArray
      el "p" $ text "What is the area?"
      pure (preambleCorrect, w * h)
    Question_Perimeter showingArray w h -> do
      preambleCorrect <- preamble w h showingArray
      el "p" $ text "What is the perimeter?"
      pure (preambleCorrect, 2 * (w + h))

  otherCorrect <- el "p" $ do
    perim :: Dynamic t (Maybe Int) <- fmap (readMaybe . T.unpack) . value <$> inputElement def
    let correct = (fmap.fmap) (rightAnswer ==) perim
    dyn_ $ ffor correct $ traverse_ $ \c -> text " " *> smile c
    pure correct

  let ready = fmap (== Just True) $ (liftA2.liftA2) (&&) preambleCorrect otherCorrect
  switchHold never <=< dyn $ ffor ready $ \r -> case r of
    False -> pure never
    True -> button "Next problem"

  where
    preamble w h = \case
      Nothing -> do
        el "p" $ text $ "You have a " <> tshow w <> "x" <> tshow h <> " array."
        pure (pure (Just True))
      Just (ShowArray size) -> do
        array size w h
        el "p" $ do
          text "What is the size of this array?"
        el "p" $ do
          width <- value <$> inputElement def
          text " x "
          height <- value <$> inputElement def
          let both :: Dynamic t (Maybe (Int, Int)) = (liftA2 . liftA2) (,) (readMaybe . T.unpack <$> width) (readMaybe . T.unpack <$> height)
              correct = (fmap.fmap) ((w, h) ==) both
          dyn_ $ ffor correct $ traverse_ $ \c -> text " " *> smile c
          pure correct

smile :: DomBuilder t m => Bool -> m ()
smile c = text $ if c then "😁" else "😕"

array :: DomBuilder t m => Int -> Int -> Int -> m ()
array size w h =
  elAttr "table" (style [border, ("border-collapse", "collapse")]) $ el "tbody" $ replicateM_ h $
    el "tr" $ replicateM_ w $
      elAttr "td" (style [border, ("width", px size), ("height", px size)]) blank
  where
    px n = tshow n <> "px"
    style vs = "style"=:(T.intercalate ";" [k <> ":" <> v | (k, v) <- vs] <> ";")
    border = ("border", "1px solid black")

tshow :: Show a => a -> T.Text
tshow = T.pack . show