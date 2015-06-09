module QFeldspar.Expression.Conversions.CodeGeneration where

import QFeldspar.MyPrelude hiding ((<>))

import Text.PrettyPrint
import qualified Data.List

import qualified QFeldspar.Type.ADT as TA
import QFeldspar.Expression.C

class Pretty a where
 pretty :: a -> Doc

instance Pretty Exp where
 pretty (Var x)     = text x
 pretty (Flt f)     = text (show f++"f")
 pretty (Wrd i)     = text (show i++"u")
 pretty (App op es) = text op <+> parens (commaCat (fmap pretty es))

instance Pretty Stmt where
 pretty (Whl e ss) = text "while" <+> parens (pretty e)
  $+$ (lbrace
   $+$ nest 2 (vcat (fmap pretty ss))
   $+$ rbrace)
 pretty (If e1 ss1 ss2) = text "if" <+> parens (pretty e1)
  $+$ (lbrace
   $+$ nest 2 (vcat (fmap pretty ss1))
   $+$ rbrace)
  $+$ text "else"
  $+$ (lbrace
   $+$ nest 2 (vcat (fmap pretty ss2))
   $+$ rbrace)
 pretty (Assign v e)      = text v <+> text "=" <+> pretty e <> semi
 pretty (Declare (v , t)) = pretty t <+> text v <> semi
 pretty (Return e)        = text "return" <+> pretty e <> semi

instance Pretty Func where
 pretty (Func ty name vs ss) =
  pretty ty <+> text name
  <+> parens (commaCat (fmap pretty vs) )
  $+$ (lbrace
   $+$ nest 2 (vcat (fmap pretty ss))
   $+$ rbrace)

instance Pretty Var where
 pretty (v,t) = pretty t <+> text v

instance Pretty TA.Typ where
  pretty t = case t of
    TA.Wrd     -> text "Wrd"
    TA.Bol     -> text "Bol"
    TA.Flt     -> text "Flt"
    TA.Cmx     -> text "Cmx"
    TA.Tpl a b -> text "Tpl" <> pretty a <> pretty b
    TA.Ary a   -> text "Ary" <> pretty a
    TA.May a   -> text "May" <> pretty a
    TA.TVr _   -> impossible
    TA.Arr _ _ -> impossible
    TA.Vec _   -> impossible

commaCat :: [Doc] -> Doc
commaCat ds = foldl1 (<>) (Data.List.intersperse (comma<>space) ds)
