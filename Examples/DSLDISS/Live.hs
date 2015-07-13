import Prelude
import System.Process








import QFeldspar.QDSL

{-
Syntax:
  GHC Haskell (almost)
  Typed Quotations
  domain-specific constructs
              =
          fully applied variables

Primitives:
- constructors and destructors for each datatype
- overloaded arithmetic operators
- overloaded comparative operators (for base types)
- coercion functions (for base types)
- bitwise operators (for Word32)

-}

-- while loop in QFeldspar
-- while :: Rep a =>
--         (a -> Bool) -> (a -> a) -> a -> a

for :: Rep a => Qt (Word32 -> a -> (Word32 -> a -> a) -> a)
for = [|| \ n x0 f ->
            snd (while (\ (i , _x) -> i < n)
                       (\ (i ,  x) -> (i + 1 , f i x))
                       (0 , x0)) ||]

fib :: Qt (Word32 -> Word32)
fib = [|| \ n -> fst ($$for n (0, 1)
                     (\ _i -> \ (a, b) -> (b, a + b))) ||]

fibCodeC :: String
fibCodeC = qdsl fib

makeExec :: IO ()
makeExec = do let mainFunction = "int main (int argc, char *argv[]) {\n"++
                                 "  unsigned int inp;\n" ++
                                 "  sscanf(argv[1],\"%d\",&inp);\n" ++
                                 "  unsigned int out;\n" ++
                                 "  out = func(inp);\n" ++
                                 "  printf (\"%d\\n\",out);\n" ++
                                 "  return 0;\n" ++
                                 " }"
              writeFile "./Examples/DSLDISS/fib.c" (fibCodeC ++ "\n" ++ mainFunction)
              _ <- runCommand
                 ("gcc -o ./Examples/DSLDISS/fib ./Examples/DSLDISS/fib.c -lm -std=c99")
              return ()

{- Above produces the following after macro expansion:

typedef struct {unsigned int fst;
                unsigned int snd;} TplWrdWrd;
typedef struct {unsigned int fst;
                TplWrdWrd    snd;} TplWrdTplWrdWrd;

unsigned int func (unsigned int v0)
{
  unsigned int v3;
  TplWrdWrd v2;
  TplWrdTplWrdWrd v1;
  v1 = (TplWrdTplWrdWrd){.fst = 0u
                        ,.snd = (TplWrdWrd)
                          {.fst = 0u
                          ,.snd = 1u}};
  while (v1.fst < v0)
  {
    v2 = v1.snd;
    v3 = v2.snd;
    v1 = (TplWrdTplWrdWrd) {.fst = v1.fst + 1u
                           ,.snd = (TplWrdWrd)
                             {.fst = v3
                             ,.snd = v2.fst + v3}};
  }
  return v1.snd.fst;
}

-}
{-
Types:
  A,B,C ::= Word32 | Float | Bool | Complex_Float
          | A -> B | (A , B) | Maybe A
          | Array_Word32 A | Vector A

-}

{-
Array Word32 a
  Constructor
   mkArr :: Word32->(Word32->a)->Array Word32 a
  Destructors
   lnArr :: Array Word32 a -> Word32
   ixArr :: Array Word32 a -> Word32 -> a
Vec a
  Constructor
   Vec :: Word32 -> (Word32 -> a) -> Vec a
  Destructors by pattern matching
-}

toVec   ::  Rep a => Qt (Array Word32 a -> Vec a)
toVec    =   [|| \a -> Vec (lnArr a) (\i -> ixArr a i) ||]

fromVec ::  Rep a => Qt (Vec a -> Array Word32 a)
fromVec  =   [|| \(Vec n g) -> mkArr n g ||]

minim   ::  Ord a => Qt (a -> a -> a)
minim    =   [|| \x y -> if x < y then x else y ||]

zipVec  ::  Qt ((a -> b -> c) -> Vec a -> Vec b -> Vec c)
zipVec   = [||  \f -> \ (Vec m g) -> \ (Vec n h) ->
                Vec ($$minim m n) (\i -> f (g i) (h i)) ||]

sumVec  ::  (Rep a, Num a) => Qt (Vec a -> a)
sumVec   = [|| \(Vec n g) -> $$for n 0 (\i x -> x + g i) ||]

dotVec  ::  (Rep a, Num a) => Qt (Vec a -> Vec a -> a)
dotVec   = [|| \u v -> $$sumVec ($$zipVec (*) u v) ||]

normVec ::  Qt (Vec Float -> Float)
normVec  = [|| \v -> sqrt ($$dotVec v v) ||]

normAry :: Qt (Array Word32 Float -> Float)
normAry = [|| \ v -> $$normVec ($$toVec v) ||]

testFusion :: String
testFusion = qdsl normAry

{-
    dotVec (Vec m g) (Vec n h)
  =
    sumVec (zipVec (*) (Vec m g) (Vec n h))
  =
    sumVec (Vec (m `minim` n) (\i -> g i * h i))
  =
    for (m `minim` n) 0 (\i x -> x + g i * h i)

-}

{- Above produces the following after macro expansion:

typedef struct {unsigned int size;
                float*       elems;} AryFlt;
typedef struct {unsigned int fst;
                float        snd;} TplWrdFlt;

float func (AryFlt v0)
{
  float v3;
  unsigned int v2;
  TplWrdFlt v1;
  v1 = (TplWrdFlt) {.fst = 0u
                   ,.snd = 0.0f};
  while (v1.fst < v0.size)
  {
    v2 = v1.fst;
    v3 = v0.elems[v2];
    v1 = (TplWrdFlt) {.fst = v2 + 1u
                     ,.snd = v1.snd + (v3 * v3)};
  }
  return sqrtf (v1.snd);
}

-}

{-
Pixel
  Constructor
   $$mkPixel :: Word32->Word32->Word32->Pixel
  Destructors
   $$red   :: Pixel -> Word32
   $$green :: Pixel -> Word32
   $$blue  :: Pixel -> Word32

Image
  Constructor
   mkImage :: Word32->Word32->
             (Word32->Word32->Pixel)->Image
  Destructors
   $$heightImage :: Image -> Word32
   $$widthImage  :: Image -> Word32
   $$getPixel :: Image->Word32->Word32->Pixel

Conversions
  $$aryToImage :: Word32 -> Word32 ->
                  Array Word32 Word32 -> Image
  $$imageToAry :: Image -> Array Word32 Word32

Compiler
  compileImageProcessor ::
    String -> Qt (Image -> Image) -> IO ()

-}
