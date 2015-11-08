module Euler where

import Data.Function
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad.State
import qualified Data.Set as S
import Palindromic

-- 1
euler1 = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1 .. 999]

-- 2
fib = 1 : 2 : zipWith (+) fib (tail fib)
euler2 = sum $ fst $ break (> 4000000) $ filter even fib

-- 3
primes = sieve [2 ..]
    where sieve (n:ns) = n : sieve (filter (\k -> k `mod` n /= 0) ns)
    

-- generate primes less than 10 ^ 6 into the file
generate_primes10x6 =
  do h10 <- openFile "primes10x6.txt" WriteMode 
  mapM_ (\n -> do hPutStrLn h10 (show n)
                  print n) 
        (takeWhile (<1000000) primes)
  hClose h10

{- -- read prime numbers into list named ""primesB"
tmp <- readFile "primes10x6.txt"
let {primesB :: [Int]; primesB = map read (lines tmp)}
-}

factorize' ms n ps@(k:ks)
    | n == k = k : ms
    | otherwise =
        let (n', r') = n `divMod` k
        in 
            if r' == 0
            then factorize' (k : ms) n' ps
            else factorize' ms n ks

factorize 1 = [1]
factorize n = factorize' [] n primes

euler3 = head $ factorize 600851475143

euler4 = maximum [n | k <- [100 .. 999], m <- [100 .. k], let n = k * m, isPalindrome n]

cmpDivs ns ms = on compare head ns ms `mappend` on compare length ns ms
addDivs ns [] = [ns]
addDivs ns (ms : mss) =
    case on compare head ns ms of
        LT -> ns : ms : mss
        GT -> ms : addDivs ns mss 
        EQ -> if on (<) length ns ms
                then ms : mss
                else ns : mss
    
lcmL ns = product $ concat $ foldr addDivs [] $ concatMap (group . factorize) ns
euler5 = lcmL [2 .. 20]

euler6 = (sum [1 .. 100] ^ 2) - sum (map (^2) [1 .. 100]) 
euler7 = primes !! 10000

num8str ="7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

adj' f n res xs = 
    let xs1 = take n xs
    in
        if length xs1 < n 
            then res
            else adj' f n ((f xs1, xs1) : res) (tail xs)

adjMuls n = adj' product n []

euler8 = maximumBy (compare `on` fst) $ adjMuls 13 $ map digitToInt num8str 
euler9 =  head [a*b*c*(25^3) | c <- [1 .. 100]
                        ,b <- [1 .. (c - 1)]
                        ,a <- [1 .. (b - 1)]
                        ,a ^ 2 + b ^ 2 == c ^ 2
                        ,a + b + c == 1000 `div` 25]

euler10 = 
    let xs1 = takeWhile (<2000000) primes
        f (n, s) n1 =
            let s1 = s + n1
            in s1 `seq` (n1, s1)
    in
       scanl' f (0,0) xs1
  
arr11str :: String  
arr11str = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08\n49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00\n81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65\n52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91\n22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80\n24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50\n32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70\n67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21\n24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72\n21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95\n78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92\n16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57\n86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58\n19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40\n04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66\n88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69\n04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36\n20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16\n20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54\n01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
        
arr11 :: [[Integer]]
arr11 = map (map read . words) $ lines arr11str
bnd11 = 19

indCol11 = [[(i,j)| i <- [0 .. bnd11]] | j <- [0 .. bnd11]]
indRDiag11L =[[(i + m, i) | i <- [0 .. bnd11 - m]] | m <- [0 .. bnd11]]
indRDiag11R =[[(i, i + m) | i <- [0 .. bnd11 - m]] | m <- [1 .. bnd11]]
indLDiag11L =[[(m - i, i) | i <- [0 .. m]] | m <- [0 .. bnd11]]
indLDiag11R =[[(bnd11 - i, m + i) | i <- [0 .. (bnd11 - m)]] | m <- [0 .. bnd11]]

arr11lines :: [[(Int, Int)]] -> [[Integer]]
arr11lines indX = [[arr11 !! i !! j | (i, j) <- l ] | l <- indX ]

maxMuls11 = maximum . map maximum . filter (not. null) . map (adjMuls 4)

euler11 = maximum $ map maxMuls11 (arr11 : map arr11lines [indCol11, indRDiag11L, indRDiag11R, indLDiag11L, indLDiag11R])

euler12 = head $ filter ((>500).snd) $ 
          map (\n -> (n , length $ nub $ map product $ subsequences $ factorize n)) 
          $ map (\n -> sum [1 .. n]) [1 .. ]
          
num13str = "37107287533902102798797998220837590246510135740250\n46376937677490009712648124896970078050417018260538\n74324986199524741059474233309513058123726617309629\n91942213363574161572522430563301811072406154908250\n23067588207539346171171980310421047513778063246676\n89261670696623633820136378418383684178734361726757\n28112879812849979408065481931592621691275889832738\n44274228917432520321923589422876796487670272189318\n47451445736001306439091167216856844588711603153276\n70386486105843025439939619828917593665686757934951\n62176457141856560629502157223196586755079324193331\n64906352462741904929101432445813822663347944758178\n92575867718337217661963751590579239728245598838407\n58203565325359399008402633568948830189458628227828\n80181199384826282014278194139940567587151170094390\n35398664372827112653829987240784473053190104293586\n86515506006295864861532075273371959191420517255829\n71693888707715466499115593487603532921714970056938\n54370070576826684624621495650076471787294438377604\n53282654108756828443191190634694037855217779295145\n36123272525000296071075082563815656710885258350721\n45876576172410976447339110607218265236877223636045\n17423706905851860660448207621209813287860733969412\n81142660418086830619328460811191061556940512689692\n51934325451728388641918047049293215058642563049483\n62467221648435076201727918039944693004732956340691\n15732444386908125794514089057706229429197107928209\n55037687525678773091862540744969844508330393682126\n18336384825330154686196124348767681297534375946515\n80386287592878490201521685554828717201219257766954\n78182833757993103614740356856449095527097864797581\n16726320100436897842553539920931837441497806860984\n48403098129077791799088218795327364475675590848030\n87086987551392711854517078544161852424320693150332\n59959406895756536782107074926966537676326235447210\n69793950679652694742597709739166693763042633987085\n41052684708299085211399427365734116182760315001271\n65378607361501080857009149939512557028198746004375\n35829035317434717326932123578154982629742552737307\n94953759765105305946966067683156574377167401875275\n88902802571733229619176668713819931811048770190271\n25267680276078003013678680992525463401061632866526\n36270218540497705585629946580636237993140746255962\n24074486908231174977792365466257246923322810917141\n91430288197103288597806669760892938638285025333403\n34413065578016127815921815005561868836468420090470\n23053081172816430487623791969842487255036638784583\n11487696932154902810424020138335124462181441773470\n63783299490636259666498587618221225225512486764533\n67720186971698544312419572409913959008952310058822\n95548255300263520781532296796249481641953868218774\n76085327132285723110424803456124867697064507995236\n37774242535411291684276865538926205024910326572967\n23701913275725675285653248258265463092207058596522\n29798860272258331913126375147341994889534765745501\n18495701454879288984856827726077713721403798879715\n38298203783031473527721580348144513491373226651381\n34829543829199918180278916522431027392251122869539\n40957953066405232632538044100059654939159879593635\n29746152185502371307642255121183693803580388584903\n41698116222072977186158236678424689157993532961922\n62467957194401269043877107275048102390895523597457\n23189706772547915061505504953922979530901129967519\n86188088225875314529584099251203829009407770775672\n11306739708304724483816533873502340845647058077308\n82959174767140363198008187129011875491310547126581\n97623331044818386269515456334926366572897563400500\n42846280183517070527831839425882145521227251250327\n55121603546981200581762165212827652751691296897789\n32238195734329339946437501907836945765883352399886\n75506164965184775180738168837861091527357929701337\n62177842752192623401942399639168044983993173312731\n32924185707147349566916674687634660915035914677504\n99518671430235219628894890102423325116913619626622\n73267460800591547471830798392868535206946944540724\n76841822524674417161514036427982273348055556214818\n97142617910342598647204516893989422179826088076852\n87783646182799346313767754307809363333018982642090\n10848802521674670883215120185883543223812876952786\n71329612474782464538636993009049310363619763878039\n62184073572399794223406235393808339651327408011116\n66627891981488087797941876876144230030984490851411\n60661826293682836764744779239180335110989069790714\n85786944089552990653640447425576083659976645795096\n66024396409905389607120198219976047599490197230297\n64913982680032973156037120041377903785566085089252\n16730939319872750275468906903707539413042652315011\n94809377245048795150954100921645863754710598436791\n78639167021187492431995700641917969777599028300699\n15368713711936614952811305876380278410754449733078\n40789923115535562561142322423255033685442488917353\n44889911501440648020369068063960672322193204149535\n41503128880339536053299340368006977710650566631954\n81234880673210146739058568557934581403627822703280\n82616570773948327592232845941706525094512325230608\n22918802058777319719839450180888072429661980811197\n77158542502016545090413245809786882778948721859617\n72107838435069186155435662884062257473692284509516\n20849603980134001723930671666823555245252804609722\n53503534226472524250874054075591789781264330331690"

num13arr :: [Integer]
num13arr = map read $ lines num13str

euler13 = take 10 $ show $ sum num13arr

collatzSeq' [1] = collatzSeq' [4,1]
collatzSeq'  xs@(1 : _) = xs
collatzSeq' xs@(n : _)
    | even n = collatzSeq' $ n `div` 2 : xs
    | otherwise = collatzSeq' $ (3 * n + 1) : xs

collatzSeq n = reverse $ collatzSeq' [n]

-- pretty number paintings :) answer 837799, 525-long chain
euler14 = scanl' (\((lm, m), _) (lc, c) -> if lc > lm then ((lc, c),(lc,c)) else ((lm,m),(lc,c))) ((0,0),(0,0)) $ map (\n -> (length $ collatzSeq n, n)) [1 .. 1000000]
    
type GridTbl = [((Int, Int), Integer)]
initGridTbl = [((0,0), 1)]

upgrTbl15 m n tbl 
    | m > n = tbl
    | otherwise = 
        let k = pointPathes tbl (m - 1, n) + pointPathes tbl (m, n - 1)
            tbl1 = if m == n
                        then ((n, n), k) : tbl
                        else ((m, n), k) : ((n, m), k) : tbl
        in upgrTbl15 (m + 1) n tbl1

pointPathes tbl (i,j) | i < 0 || j < 0 = 0
pointPathes tbl p = 
    case lookup p tbl of
      Just k -> k
      Nothing -> 
        let ((n, _), _) = maximum tbl
            tbl1 = upgrTbl15 0 (n + 1) tbl
        in pointPathes tbl1 p
        
euler15 = pointPathes initGridTbl (20,20)
euler16 = sum $ map digitToInt $ show $ 2 ^ 1000

num18arr = [[75],[95,64],[17,47,82],[18,35,87,10],[20,4,82,47,65],[19,1,23,75,3,34],[88,2,77,73,7,63,67],[99,65,4,28,6,16,70,92],[41,41,26,56,83,40,80,70,33],[41,48,72,33,47,32,37,16,94,29],[53,71,44,65,25,43,91,52,97,51,14],[70,11,33,28,77,73,17,78,39,68,17,57],[91,71,52,38,17,14,91,43,58,50,27,29,48],[63,66,4,68,89,53,67,30,73,16,69,87,40,31],[4,62,98,27,23,9,70,98,73,93,38,53,60,4,23]]

initTbl18 = [((0, 0), 75)] :: GridTbl

upgrTbl18 :: Int -> Int -> GridTbl -> GridTbl
upgrTbl18 n m tbl 
  | m > n || n > 14 = tbl
  | otherwise =
        let k = max (pointCost tbl (n - 1, m - 1)) (pointCost tbl (n - 1, m))
        in upgrTbl18 n (m + 1) $ ((n, m), k + num18arr !! n !! m) : tbl
        
pointCost :: GridTbl -> (Int, Int) -> Integer
pointCost tbl p@(i, j)
  | i < 0 || j < 0 || j > i = 0
  | otherwise = 
        case lookup p tbl of
            (Just k) -> k
            Nothing -> 
                let ((n, _), _) = maximum tbl
                    tbl1 = upgrTbl18 (n + 1) 0 tbl
                in pointCost tbl1 p
                
euler18 = maximumBy (compare `on` snd) $ foldl (\tbl _ -> let n = (fst . fst . maximum) tbl in upgrTbl18 (n+1) 0 tbl) initTbl18 [0 .. 14]

yearLp =[31,29,31,30,31,30,31,31,30,31,30,31] :: [Int]
year   =[31,28,31,30,31,30,31,31,30,31,30,31] :: [Int]
century = concat $ concat $ replicate (100 `div` 4) [year, year, year, yearLp]
days = cycle [1 .. 7] :: [Int]

euler19' res [] _ = res
euler19' res (m:ms) ds@(d:_)
    | d == 7 = euler19' (d:res) ms (drop m ds)
    | otherwise = euler19' res ms (drop m ds)
    
euler19 = length $ euler19' [] century (drop (sum yearLp) days)

euler20 = sum $ map digitToInt $ show $ product [1 .. 100]

propFactors n = (sort . filter (<n) . nub . map product . subsequences . factorize) n

sumPropDiv = sum . propFactors

euler21' amics [] = amics
euler21' amics (n:ns) = 
    let m = sumPropDiv n
    in
        if m /= n && sumPropDiv m == n
            then euler21' (m : n : amics) (filter (/= m) ns)
            else euler21' amics ns
    
euler21 = sum $ euler21' [] [2 .. 10000]

euler22 =
  do
    lst <- readFile "p022_names.txt"
    let lst' = foldr (\c str -> if elem c ['"',','] then (' ' : str) else (c:str)) [] lst
    let lst'' = sort $ words lst'
    let lst''' = map (map ((flip $ on ((-) . (+1)) ord) 'A')) lst''
    return $ sum $ zipWith (*) [1 ..] $ map sum lst'''

abundants = filter (\n -> sumPropDiv n > n) [2 .. 28127]

elemS x xs = 
    case dropWhile (<x) xs of
        [] -> Nothing
        (y : ys) -> if y > x then Just False else Just True
        
--lenAbun = length abundants - 1 

abundable :: Int -> State (S.Set Int, Int) Bool
abundable n = 
  do
    (abuns, k) <- get
    if k < n - 12
        then do let (ks, (k':_)) = break (k <) abundants
                let abuns' = foldr S.insert abuns (map (k' +) (k' : ks))
                put (abuns', k')
                abundable n
        else return $ S.member n abuns

addAbuns abuns k = 
    let (ns, (n:_)) = break (k <) abundants
    in foldr S.insert abuns (map (n +) (n : ns))
    
--euler23 = execState (filterM (\n -> not <$> abundable n) [1 .. 123]) (S.empty,0)
euler23 = scanl' (\(n', s) n -> (n, s + n)) (0,0) $ evalState (filterM (\n -> not <$> abundable n) [1 .. 28127]) (S.empty,0)
    
euler24' = [[a0,a1,a2,a3,a4,a5,a6,a7,a8,a9] | let digs = [0 .. 9]
                                                , a0 <- digs
                                                , a1 <- digs \\ [a0]
                                                , a2 <- digs \\ [a0, a1]
                                                , a3 <- digs \\ [a0, a1, a2]
                                                , a4 <- digs \\ [a0, a1, a2, a3]
                                                , a5 <- digs \\ [a0, a1, a2, a3, a4]
                                                , a6 <- digs \\ [a0, a1, a2, a3, a4, a5]
                                                , a7 <- digs \\ [a0, a1, a2, a3, a4, a5, a6]
                                                , a8 <- digs \\ [a0, a1, a2, a3, a4, a5, a6, a7]
                                                , a9 <- digs \\ [a0, a1, a2, a3, a4, a5, a6, a7, a8]]
                                                
euler24 = concatMap show $ euler24' !! (1000000 - 1)

fib1 = 1 : 1 : zipWith (+) fib1 (tail fib1)

euler25' = takeWhile (\(_, l) -> l < 1000) $ scanl' (\(i, _) n -> (i+1, length (show n))) (0,0) fib1
euler25 = (1+) $ length $ takeWhile (<1000) $ map (length . show) fib1

--grow10 n m = if n >= m then n else grow10 (n*10) m

recurCycle' rs n m = 
  let p@(d, r) = n `divMod` m
  in
      if r == 0 then []
      else
          if elem (r, d) rs
            then (r, d) : reverse (takeWhile (\(r', _) -> r' /= r) rs)  
            else recurCycle' ((r, d) : rs) (r * 10) m

recurCycle n m = concatMap show $ snd $ unzip $ recurCycle' [] n m   

euler26 = maximumBy (compare `on` fst) $ zip (map (length . recurCycle 1) [2 .. 999]) [2 .. 999]

isPrime n = 
  let (ps1, ps2) = span (< n) primes
  in head ps2 == n

-- n² + an + b
quadrPrimeSeq a b = takeWhile isPrime $ map (\n -> n ^ 2 + a * n + b) [0..]

qPSeqs = [(a, b, n) | a <- [-999 .. 999], b <- takeWhile (< 1000) primes
                    , let n = length (quadrPrimeSeq a b) ]
                    
euler27 = scanl' (\(m@(_,_,n1), _) cur@(_,_,n2) -> 
                    if n2 > n1 then (cur, cur) else (m, cur))
                 ((0,0,0), (0,0,0))
                 qPSeqs

euler28' n = 1 :
  concatMap (\n -> let n' = n ^ 2 
                   in tail $ scanl' (+) n' (replicate 4 (n + 1)))
            [1,3 .. (n - 2)]
            
euler28 = sum $ euler28' 1001

euler29' n = nub [a ^ b | a <- [2 .. n], b <- [2 .. n]]

euler29 = length  $ euler29' 100

fifthesSum n = sum $ map ((^5) . digitToInt) $ show n

euler30 = sum $ filter (\n -> n == fifthesSum n) [2 .. 299999]
-- sum [4150,4151,54748,92727,93084,194979] = 443839

britCoins = [1, 2, 5, 10, 20, 50, 100, 200]

euler31' 0 res _ = res
euler31' n _ _ | n < 0 = []
euler31' n  res  ls = concat [ euler31' (n-k) (map (k:) res) (snd $ span (< k) ls) | k <- ls ]

euler31 = length $ euler31' 200 [[]] britCoins
-- 73682

euler32' = [[k, n `div` k, n] | n <- [2 .. 9876]
                             , k <- filter (\m -> m > 1 
                                           && fromIntegral m < sqrt (fromIntegral n))
                                           (propFactors n)]
euler32'' = filter (\xs -> (sort . concatMap show) xs == "123456789") euler32'

euler32 = sum $ nub $ map (!!2) euler32''
-- 45228

euler33' = [[n, m, d] | n <- [1 .. 9], m <- [(n + 1) .. 9], d <- [1 .. 9]
                      , (n * 10 + d) * m == (m * 10 + d) * n 
                       || (d * 10 + n) * m == (d * 10 + m) * n
                       || (n * 10 + d) * m == (d * 10 + m) * n]

euler33 = 
  let (n, d) = unzip $ map (\(x1 : x2 : _) -> (x1, x2)) euler33'
      (n', d') = (concatMap factorize n, concatMap factorize d)
      (n2, d2) = (n' \\ d', d' \\ n')
  in product d2
-- 100 
fac n | n <= 0 = 1
fac n = product [1 .. n]

fac10 = map fac [0 .. 9]

curious n = sum (map ((fac10 !!) . digitToInt) (show n)) == n

euler34' = scanl (\(s, _) n -> if curious n then (n : s, n) else (s, n)) ([],0) [3 .. 40585]
--40585,145
euler34 = sum [40585,145]

cycleS s =
 let n = length s
     ss = s ++ s
 in nub [take n (drop k ss) | k <- [0 .. (n - 1)]]

cycleNum :: Int -> [Int] 
cycleNum n = map read $ cycleS $ show n

euler35 = 
  do
    tmp <- readFile "primes10x6.txt"
    let primesBs = lines tmp
    let euler35' = scanl (\(s, n, _) l -> 
                          if elem '0' l 
                            then (s, n, l)
                            else
                              let ls = cycleS l
                              in if all ((flip elem) primesBs) ls
                                 then 
                                   let s2 = s `union` ls 
                                   in (s2, length s2, l)
                                 else (s, n, l)) 
                       ([],0,"") primesBs                       
    mapM_ (putStrLn . show) $ scanl (\_ (_, n, l) -> (n, l)) (0, "") euler35' 
-- 55
                       
revBin m 
 | m == 0 = [0]
 | otherwise = 
    unfoldr (\n -> let (n', r) = n `divMod` 2 
                   in if n > 0 
                        then (Just (r, n')) 
                        else Nothing)
             m

isPalindrome2 n =
  let n' = revBin n
  in isPalindrome n && (n' == reverse n')
  
euler36 = scanl (+) 0 $ filter isPalindrome2 [1 .. 999999]

truncates s = init (tails s) ++ tail (inits s)

euler37 =
  do
    tmp <- readFile "primes10x6.txt"
    let primesBs = lines tmp
    let primesBs2 = dropWhile (\s -> length s < 2) primesBs
    let euler37' =
          scanl (\(s, n, _) l ->
                if elem '0' l 
                  then (s, n, l)
                  else
                    if all ((flip elem) primesBs) (truncates l)
                      then (l : s, n + 1, l)
                      else (s, n, l))
            ([],0,"") primesBs2
    let (euler37'', euler37''') = break (\(_, n, _) -> n == 11) euler37'
    mapM_ (putStrLn . show) euler37''
    let (s, _, _) = (head euler37''')
    (putStrLn . show) s
    print $ sum (map read s)
    
findSM n [] = Nothing
findSM n (m:ms)
  | n == m = Just n
  | m > n = Nothing
  | otherwise = findSM n ms
  
getArSeqs3 (x : xs) = 
  catMaybes 
            [ do let d = x2 - x
                 x3 <- findSM (x2 + d) xs 
                 return [x, x2, x3]
            | x2 <- xs ]

euler49' =
  let primes4 = takeWhile (<10000) $ dropWhile (<1000) primes
  in concatMap getArSeqs3 $ init $ tails primes4
  
euler49 = [ns |ns <- euler49', let [s1, s2, s3] = map (sort . show) ns, s1 == s2 && s2 == s3]
-- 2969,6299,9629

-- 51
xorStr' rs [] [] = reverse rs
xorStr' rs (a:as) (b:bs) = 
    if a == b 
        then xorStr' (a : rs) as bs
        else xorStr' ('_':rs) as bs
xorStr' _ _ _ = []

-- 55
euler55 = length $ filter isLyshrel1 [1 .. 9999]