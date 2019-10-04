{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

import           Data.List
import           Data.Sort
import           Control.Egison hiding (Integer)
import qualified Control.Egison as M
import           Debug.Trace



-- Literal matcher = Integer matcher
data Literal = Literal
instance Integral a => Matcher Literal a

instance Integral a => ValuePat Literal a where
  valuePat f = Pattern (\ctx _ tgt -> [MNil | f ctx == tgt])

-- Stage matcher = Integer matcher
data Stage = Stage
instance Integral a => Matcher Stage a

instance Integral a => ValuePat Stage a where
  valuePat f = Pattern (\ctx _ tgt -> [MNil | f ctx == tgt])

-- Matchers for assignments
type TaggedLiteral = (Integer, Integer) -- a tuple of a variable and a stage

data Assign = Deduced TaggedLiteral [TaggedLiteral]
            | Guessed TaggedLiteral
  deriving (Show)

data Assignment = Assignment
instance Matcher Assignment Assign

deduced :: Pattern TaggedLiteral (Pair Literal Stage) ctx xs
        -> Pattern [TaggedLiteral] (Multiset (Pair Literal Stage)) (ctx :++: xs) ys
        -> Pattern Assign Assignment ctx (xs :++: ys)
deduced p1 p2 = Pattern (\_ _ tgt -> case tgt of
                                       Deduced l ls -> [twoMAtoms (MAtom p1 (Pair Literal Stage) l)
                                                                  (MAtom p2 (Multiset (Pair Literal Stage)) ls)]
                                       _ -> [])

guessed :: Pattern TaggedLiteral (Pair Literal Stage) ctx xs
        -> Pattern Assign Assignment ctx xs
guessed p1 = Pattern (\_ _ tgt -> case tgt of
                                    Guessed l -> [oneMAtom (MAtom p1 (Pair Literal Stage) l)]
                                    _ -> [])

whichever :: Pattern TaggedLiteral (Pair Literal Stage) ctx xs
          -> Pattern Assign Assignment ctx xs
whichever p1 = Pattern (\_ _ tgt -> case tgt of
                                      Deduced l _ -> [oneMAtom (MAtom p1 (Pair Literal Stage) l)]
                                      Guessed l -> [oneMAtom (MAtom p1 (Pair Literal Stage) l)])

--
-- VSIDS
--
toCNF :: [[Integer]] -> [([Integer], [Integer])]
toCNF cs = map (\c -> (c, c)) cs

fromCNF ::  [([Integer], [Integer])] -> [[Integer]]
fromCNF cnf = map (\(c1, _) -> c1) cnf

initVars :: [Integer] -> [(Integer, Integer)]
initVars vs = map (\v -> (negate v, 0)) vs ++ map (\v -> (v, 0)) vs

addVars :: [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
addVars vs vars =
  matchDFS (vs, vars) (Pair (List Literal) (List (Pair Literal M.Integer)))
    [[mc| pair nil _ => sortBy (\(_, c1) (_, c2) -> opposite (compare c1 c2)) vars |],
     [mc| pair (cons $v $vs2) (join $hs (cons (pair #v $c) $ts)) =>
          addVars vs2 (hs ++ (v, c + 1) : ts) |]]
 where
   opposite LT = GT
   opposite GT = LT
   opposite EQ = EQ

deleteVar :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
deleteVar v vars =
  matchDFS vars (Multiset (Pair Literal M.Integer))
    [[mc| cons (pair #v _) (cons (pair #(negate v) _) $vars2) => vars2 |]]

--
-- Utility functions for literals and cnfs
--
getStage :: Integer -> [Assign] -> Integer
getStage l trail =
  matchDFS trail (List Assignment)
    [[mc| join _ (cons (whichever (pair #(negate l) $s)) _) => s |]]

deleteLiteral :: Integer -> [([Integer], [Integer])] -> [([Integer], [Integer])]
deleteLiteral l cnf =
  map (\(c1, c2) -> (matchAll c1 (Multiset Literal)
                       [[mc| cons (& (not #l) $m) _ => m |]],
                     c2))
      cnf

deleteClausesWith :: Integer -> [([Integer], [Integer])] -> [([Integer], [Integer])]
deleteClausesWith l cnf =
  matchAll cnf (Multiset (Pair (Multiset Literal) (Multiset Literal)))
    [[mc| cons (& (pair (not (cons #l _)) _) $c) _ => c |]]

assignTrue :: Integer -> [([Integer], [Integer])] -> [([Integer], [Integer])]
assignTrue l cnf =
  deleteLiteral (negate l) (deleteClausesWith l cnf)

--
-- Unit propagation
--
unitPropagate :: Integer -> [([Integer], [Integer])] -> [Assign] -> ([([Integer], [Integer])], [Assign])
unitPropagate stage cnf trail = unitPropagate' stage cnf trail trail

unitPropagate' :: Integer -> [([Integer], [Integer])] -> [Assign] -> [Assign] -> ([([Integer], [Integer])], [Assign])
unitPropagate' stage cnf trail otrail =
  matchDFS trail (List Assignment)
    [[mc| cons (whichever (pair $l _)) $trail2 => unitPropagate' stage (assignTrue l cnf) trail2 otrail |],
     [mc| _ => unitPropagate'' stage cnf otrail |]]
   
unitPropagate'' :: Integer -> [([Integer], [Integer])] -> [Assign] -> ([([Integer], [Integer])], [Assign])
unitPropagate'' stage cnf trail =
  matchDFS cnf (Multiset (Pair (Multiset Literal) (Multiset Literal)))
    [[mc| cons (pair nil _) _ => (cnf, trail) |],
     [mc| cons (pair (cons $l nil) (cons #l $rs)) _ =>
          unitPropagate'' stage (assignTrue l cnf) 
                          ([(Deduced (l, stage) (map (\r -> (r, (getStage r trail))) rs))] ++ trail) |],
     [mc| _ => (cnf, trail) |]]

--
-- Learning
--

learn :: Integer -> [(Integer, Integer)] -> [Assign] -> (Integer, [Integer])
learn stage cl trail =
  matchDFS (trail, cl) (Pair (List Assignment) (Multiset (Pair Literal M.Integer))) -- must be matchDFS
    [[mc| pair _ (not (cons (pair _ #stage) (cons (pair _ #stage) _))) =>
          (minimum (map (\(_, c) -> c) cl), map (\(l, _) -> l) cl) |],
     [mc| pair (join _ (cons (deduced (pair $l #stage) $ds) $trail2))
               (cons (pair #(negate l) #stage) $rs) =>
          learn stage (union rs ds) trail2 |]]

--
-- Backjumping
--

backjump :: Integer -> [Assign] -> [Assign]
backjump stage trail =
  matchDFS trail (List Assignment)
    [[mc| join _ (& (cons (guessed (pair _ #stage)) _) $trail2) => trail2 |],
     [mc| _ => [] |]]

--
-- Guess
--

guess vars trail =
  matchDFS (vars, trail) (Pair (List (Pair Literal M.Integer)) (List Assignment)) -- must be matchDFS
    [[mc| pair (join _ (cons (pair $l _) _))
               (not (join _ (cons (whichever (pair (| #l #(negate l)) _)) _))) =>
          negate l |]]

--
-- CDCL main
--

cdcl :: [Integer] -> [[Integer]] -> Bool
cdcl vars cnf = cdcl' 0 0 (initVars vars) (toCNF cnf) []

cdcl' :: Integer -> Integer -> [(Integer, Integer)] -> [([Integer], [Integer])] -> [Assign] -> Bool
cdcl' count stage vars cnf trail =
  let (cnf2, trail2) = unitPropagate stage cnf trail in
--  let (cnf2, trail2) = unitPropagate stage cnf (trace (show trail) trail) in -- debug
    matchDFS (cnf2, trail2) (Pair (Multiset (Pair (Multiset Literal) (Multiset Literal))) (List Assignment))
      [[mc| pair nil _ => True |],
       [mc| pair (cons (pair nil $cc) _) (join _ (cons (guessed (pair $l #stage)) $trail3)) =>
            let (s, lc) = learn stage (map (\l -> (l, (getStage l trail2))) cc) trail2 in
            let trail4 = backjump s trail3 in
              cdcl' (count + 1) s (addVars lc vars) ((lc, lc):cnf) trail4 |],
       [mc| pair (cons (pair nil $cc) _) _ => False |],
       [mc| _ =>
            let g = guess vars trail2 in
              cdcl' (count + 1) (stage + 1) vars cnf (Guessed (g, stage + 1):trail2) |]]

main = do
--  putStrLn $ show $ cdcl [] []
--  putStrLn $ show $ cdcl [] [[]]
--  putStrLn $ show $ cdcl [1] [[1]]
--  putStrLn $ show $ cdcl [1,2] [[1],[1,2]]
--  putStrLn $ "Problem 20"
--  putStrLn $ show $ cdcl [1..20] problem20 -- 0.293 sec
  putStrLn $ "Problem 50"
  putStrLn $ show $ cdcl [1..50] problem50 -- 2.570 sec

problem20 =
  [[4,-18,19],[3,18,-5],[-5,-8,-15],[-20,7,-16],[10,-13,-7],[-12,-9,17],[17,19,5],[-16,9,15],[11,-5,-14],[18,-10,13],[-3,11,12],[-6,-17,-8],[-18,14,1],[-19,-15,10],[12,18,-19],[-8,4,7],[-8,-9,4],[7,17,-15],[12,-7,-14],[-10,-11,8],[2,-15,-11],[9,6,1],[-11,20,-17],[9,-15,13],[12,-7,-17],[-18,-2,20],[20,12,4],[19,11,14],[-16,18,-4],[-1,-17,-19],[-13,15,10],[-12,-14,-13],[12,-14,-7],[-7,16,10],[6,10,7],[20,14,-16],[-19,17,11],[-7,1,-20],[-5,12,15],[-4,-9,-13],[12,-11,-7],[-5,19,-8],[1,16,17],[20,-14,-15],[13,-4,10],[14,7,10],[-5,9,20],[10,1,-19],[-16,-15,-1],[16,3,-11],[-15,-10,4],[4,-15,-3],[-10,-16,11],[-8,12,-5],[14,-6,12],[1,6,11],[-13,-5,-1],[-7,-2,12],[1,-20,19],[-2,-13,-8],[15,18,4],[-11,14,9],[-6,-15,-2],[5,-12,-15],[-6,17,5],[-13,5,-19],[20,-1,14],[9,-17,15],[-5,19,-18],[-12,8,-10],[-18,14,-4],[15,-9,13],[9,-5,-1],[10,-19,-14],[20,9,4],[-9,-2,19],[-5,13,-17],[2,-10,-18],[-18,3,11],[7,-9,17],[-15,-6,-3],[-2,3,-13],[12,3,-2],[-2,-3,17],[20,-15,-16],[-5,-17,-19],[-20,-18,11],[-9,1,-5],[-19,9,17],[12,-2,17],[4,-16,-5]]

problem50 =
  [[18,-8,29],[-16,3,18],[-36,-11,-30],[-50,20,32],[-6,9,35],[42,-38,29],[43,-15,10],[-48,-47,1],[-45,-16,33],[38,42,22],[-49,41,-34],[12,17,35],[22,-49,7],[-10,-11,-39],[-28,-36,-37],[-13,-46,-41],[21,-4,9],[12,48,10],[24,23,15],[-8,-41,-43],[-44,-2,-35],[-27,18,31],[47,35,6],[-11,-27,41],[-33,-47,-45],[-16,36,-37],[27,-46,2],[15,-28,10],[-38,46,-39],[-33,-4,24],[-12,-45,50],[-32,-21,-15],[8,42,24],[30,-49,4],[45,-9,28],[-33,-47,-1],[1,27,-16],[-11,-17,-35],[-42,-15,45],[-19,-27,30],[3,28,12],[48,-11,-33],[-6,37,-9],[-37,13,-7],[-2,26,16],[46,-24,-38],[-13,-24,-8],[-36,-42,-21],[-37,-19,3],[-31,-50,35],[-7,-26,29],[-42,-45,29],[33,25,-6],[-45,-5,7],[-7,28,-6],[-48,31,-11],[32,16,-37],[-24,48,1],[18,-46,23],[-30,-50,48],[-21,39,-2],[24,47,42],[-36,30,4],[-5,28,-1],[-47,32,-42],[16,37,-22],[-43,42,-34],[-40,39,-20],[-49,29,6],[-41,-3,39],[-16,-12,43],[24,22,3],[47,-45,43],[45,-37,46],[-9,26,5],[-3,23,-13],[5,-34,13],[12,39,13],[22,50,37],[19,9,46],[-24,8,-27],[-28,7,21],[8,-25,50],[20,50,4],[27,36,13],[26,31,-25],[39,-44,-32],[-20,41,-10],[49,-28,35],[1,44,34],[39,35,-11],[-50,-42,-7],[-24,7,47],[-13,5,-48],[-9,-20,-23],[2,17,-19],[11,23,21],[-45,30,15],[11,26,-24],[38,33,-13],[44,-27,-7],[41,49,2],[-18,12,-37],[-2,12,-26],[-19,7,32],[-22,11,33],[8,12,-20],[16,40,-48],[-2,-24,-11],[26,-17,37],[-14,-19,46],[5,47,36],[-29,-9,19],[32,4,28],[-34,20,-46],[-4,-36,-13],[-15,-37,45],[-21,29,23],[-6,-40,7],[-42,31,-29],[-36,24,31],[-45,-37,-1],[3,-6,-29],[-28,-50,27],[44,26,5],[-17,-48,49],[12,-40,-7],[-12,31,-48],[27,32,-42],[-27,-10,1],[6,-49,10],[-24,8,43],[23,31,1],[11,-47,38],[-28,26,-13],[-40,12,-42],[-3,39,46],[17,41,46],[23,21,13],[-14,-1,-38],[20,18,6],[-50,20,-9],[10,-32,-18],[-21,49,-34],[44,23,-35],[40,-19,34],[-1,6,-12],[6,-2,-7],[32,-20,34],[-12,43,-29],[24,2,-49],[10,-4,40],[11,5,12],[-3,47,-31],[43,-23,21],[-41,-36,-50],[-8,-42,-24],[39,45,7],[7,37,-45],[41,40,8],[-50,-10,-8],[-5,-39,-14],[-22,-24,-43],[-36,40,35],[17,49,41],[-32,7,24],[-30,-8,-9],[-41,-13,-10],[31,26,-33],[17,-22,-39],[-21,28,3],[-14,46,23],[29,16,19],[42,-32,-44],[-24,10,23],[-1,-32,-21],[-8,-44,-39],[39,11,9],[19,14,-46],[46,44,-42],[37,23,-29],[32,25,20],[14,-43,-12],[-36,-18,46],[14,-26,-10],[-2,-30,5],[6,-18,46],[-26,2,-44],[20,-8,-11],[-31,3,16],[-22,-9,39],[-49,44,-42],[-45,-44,31],[-31,50,-11],[-32,-46,2],[-6,-7,17],[19,-32,48],[39,20,-10],[-22,-37,38],[-31,9,-48],[40,12,7],[-24,-4,9],[-22,49,33],[-12,43,10],[25,-30,-10],[46,47,31],[13,27,-7],[-45,32,-35],[-50,34,9],[2,34,30],[3,16,2],[-18,45,-12],[33,37,10],[43,7,-18],[-22,44,-19],[-31,-27,-42],[-3,-40,8],[-23,-31,38]]
