{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.Show.GADTHigherOrder
       () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.Utils.TemplateHaskell
import QFeldspar.Expression.GADTHigherOrder as FGHO

infixr 5 <++>
(<++>) :: (Monad m , Functor m) =>
          m String -> m String -> m String
m <++> n = do m' <- m
              n' <- n
              return ("(" ++ m' ++ ")  (" ++ n' ++ ")")

infixr 5 <$+>
(<$+>) :: (Monad m , Functor m) =>
          m String -> m String -> m String
m <$+> n = do m' <- m
              n' <- n
              return (m' ++ " (" ++ n' ++ ")")

showM :: forall g t.
         Exp g t -> State Word32 String
showM e = case e of
  Tmp  x    -> pure x
  _         -> $(recAppMQ 'e ''Exp ( (\ s -> [| pure s |]) .  show . stripNameSpace) ['Tmp]
    [| id |] [| (<$+>) |] [| (<++>) |] (const id)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| showMF |]
    | matchQ tt [t| Exp t t |]            -> [| showM  |]
    | otherwise                           -> [| (pure . show) |]))

showMF :: (Exp g a -> Exp g b) -> State Word32 String
showMF f = do i  <- getState
              put (i+1)
              let v = "x" ++ show i
              f' <- showM (f (Tmp v))
              pure  ("\n (\\ "++ v
                     ++ " -> (" ++ f'
                     ++ "))")

instance Show (Exp g t) where
    show e = evalState (showM e) 0

instance Show (Exp g a -> Exp g b) where
    show e = evalState (showMF e) 0
