import Data.Bits (xor)
import Control.Monad.State.Strict (gets, modify, execState, MonadState(get), State) 

data Env = Env { instructions :: [Int], a :: Int, b :: Int, c :: Int, ic :: Int, output :: [Int]}
 deriving Show

adv, bxl, bst, jnz, bxc, out, bdv, cdv :: State Env ()
adv = getOp >>= \op -> modify (\env -> env { a = a env `div` (2 ^ combo op env), ic = 2 + ic env })
bxl = getOp >>= \op -> modify (\env -> env { b = b env `xor` op, ic = 2 + ic env })
bst = getOp >>= \op -> modify (\env -> env { b = combo op env `mod` 8, ic = 2 + ic env })
jnz = getOp >>= \op -> modify (\env -> env { ic = if a env /= 0 then op else 2 + ic env })
bxc = getOp >>= \op -> modify (\env -> env { b = b env `xor` c env, ic = 2 + ic env })
out = getOp >>= \op -> modify (\env -> env { ic = 2 + ic env, output = combo op env `mod` 8 : output env})
bdv = getOp >>= \op -> modify (\env -> env { b = a env `div` (2 ^ combo op env), ic = 2 + ic env })
cdv = getOp >>= \op -> modify (\env -> env { c = a env `div` (2 ^ combo op env), ic = 2 + ic env })

runProgram :: State Env ()
runProgram = do
    env <- get
    if ic env >= length (instructions env) 
    then return ()
    else do
      i <- gets (\env -> instructions env !! ic env) 
      [adv, bxl, bst, jnz, bxc, out, bdv, cdv] !! i
      runProgram

getOp :: State Env Int
getOp = gets (\env -> instructions env !! (ic env + 1))  

combo :: Int -> (Env -> Int)
combo i | i `elem` [0..3] = const i
        | i == 4          = a
        | i == 5          = b
        | i == 6          = c

testInput :: ((Int, Int, Int), [Int])
testInput = ((51571418, 0, 0), [2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0]) 

solve1 :: (Int, Int, Int) -> [Int] -> String
solve1 (initA, initB, initC) instrs = 
  let e = execState runProgram (Env instrs initA initB initC 0 []) 
  in show (reverse $ output e)

solve2 :: [Integer] -> Integer
solve2 = minimum . flip f 0 . reverse

-- | Obvious recursive function
f :: [Integer] -> Integer -> [Integer]
f []          oldA  = [oldA]
f (r : rOuts) oldA  = concat
  [ f rOuts (8 * oldA + curr3)
  | curr3 <- [0..7]
  , ((curr3 `xor` 5) `xor` ((8 * oldA + curr3) `div` (2 ^ (curr3 `xor` 1)))) `mod` 8 == r
  ]