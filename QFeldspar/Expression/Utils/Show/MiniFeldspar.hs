{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.Show.MiniFeldspar
       () where

import QFeldspar.MyPrelude
import QFeldspar.Singleton
import QFeldspar.Expression.Utils.TemplateHaskell
import QFeldspar.Expression.MiniFeldspar
import qualified QFeldspar.Type.GADT as TFG
import QFeldspar.Environment.Typed hiding (fmap)

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

data TT a b = TT {unTT :: a}

showM :: forall g t.
         Exp g t -> State Word32 String
showM e = case e of
  AppV v es -> (pure ("AppV " ++ show v)) <$+> fmap show (TFG.mapMC (sinTyp v) (fmap TT . showM) es)
  Tmp  x    -> pure x
  _         -> $(recAppMQ 'e ''Exp ( (\ s -> [| pure s |]) .  show . stripNameSpace) ['AppV,'Tmp]
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

instance Show (Env (TT String) r') where
    show Emp        = "Emp"
    show (Ext s ss) = "\n Ext (" ++ unTT s ++ ") (" ++ show ss ++ ")"
