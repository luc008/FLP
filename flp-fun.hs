-- FLP 1. projekt - Haskell
-- fun-flp.hs
-- Lucia Mária Šmotlákova | xsmotl00

-- Import
import System.Environment (getArgs)
import Data.List (sort, group, transpose, minimumBy, partition )
import Data.Char (isDigit, isSpace)
import Data.Ord(comparing)

-- Tree Data Structure
data Tree i t  = EmptyTree
               | Node i t  (Tree i t) (Tree i t)
               | Leaf String
               deriving (Show)

--myTree :: Tree Int Double
--myTree = Node 0 5.5 (Leaf "TriedaA") (Node 1 3.0 (Leaf "TriedaB") (Leaf "TriedaC"))

---- TASK 1 FUNCTIONS ----

-- Classifier funcion 
classify :: Tree Int Double -> [Double] -> String
classify EmptyTree _ = []
classify (Leaf label) _ = label
classify (Node i t l r) inputData   
         | (inputData !! i) <= t = classify l inputData
         | otherwise = classify r inputData

-- Classify for all input data
classifyAll :: Tree Int Double -> [[Double]] -> [String]
classifyAll _ [] = []
classifyAll tree (x:xs) = classify tree x : classifyAll tree xs

-- Split based on delimiter
splitBy :: Char -> String -> [String]
splitBy delimiter str = words [if c == delimiter then ' ' else c | c <- str]

-- Convert [String] into [[Double]] for classifying
convert :: [String] -> [[Double]]
convert = map (map read . splitBy ',')

-- Count space before Node/Leaf
countSpace :: String -> Int
countSpace = length . takeWhile isSpace

-- Parse each line of input and create Node/Leaf (also holds info about "depth")
parseLine :: String -> (Int, Tree Int Double)
parseLine line =
    let numSpace = countSpace line
        cleanedStr = dropWhile isSpace line
        parts = splitBy ':' cleanedStr
    in case parts of
        ("Node" : indexPart : thresholdPart : _) ->  
            let index = read (filter isDigit indexPart) :: Int
                threshold = read (dropWhile isSpace thresholdPart) :: Double
            in (numSpace, Node index threshold EmptyTree EmptyTree)

        ("Leaf" : rest) ->  
            let label = unwords rest
            in (numSpace, Leaf label)

        _ -> error $ "Invalid tree format: " ++ line

-- Build Tree from parsed lines
buildTree :: [(Int, Tree Int Double)] -> (Tree Int Double, [(Int, Tree Int Double)])
buildTree [] = (EmptyTree, [])
buildTree ((_, current):rest) = 
    case current of
        Node index threshold EmptyTree EmptyTree -> 
            let (leftSubtree, restAfterLeft) = buildTreeHelper rest
                (rightSubtree, restAfterRight) = buildTreeHelper restAfterLeft
            in (Node index threshold leftSubtree rightSubtree, restAfterRight)

        Leaf label -> (Leaf label, rest)

        _ -> (EmptyTree, rest)

-- Helper function, returns tree + remaining part of parsed lines
buildTreeHelper :: [(Int, Tree Int Double)] -> (Tree Int Double, [(Int, Tree Int Double)])
buildTreeHelper [] = (EmptyTree, [])
buildTreeHelper ((depth, current):rest) =
    let (subtree, remaining) = buildTree ((depth, current):rest)
    in (subtree, remaining)

---- TASK 2 FUNCTIONS ----

-- Compute threshold based on average value
-- averageThreshold :: [String] -> Double
-- averageThreshold strList = 
--    let numList = map read strList :: [Double]  
--        sortedList = sort numList
--    in (head sortedList + last sortedList) / 2

-- Compute threshold based on median value
-- medianThreshold :: [String] -> Double
-- medianThreshold strList =
--    let numList = map read strList :: [Double] 
--        sortedList = sort numList 
--        n = length sortedList 
--    in if odd n 
--        then sortedList !! (n `div` 2)  -- Return the middle element
--        else let mid1 = sortedList !! (n `div` 2 - 1)  
--                 mid2 = sortedList !! (n `div` 2) 
--             in (mid1 + mid2) / 2 

-- Proces input so it looks like ([[Features]], [Labels])
processData :: [String] -> ([[Double]], [String])
processData input =
    let transposed = transpose (map (splitBy ',') input)  
        features = init transposed                      
        labels = last transposed                       
    in (map (map read) features, labels)

-- Compute Gini index
giniIndex :: [String] -> Double
giniIndex labels =
    let total = fromIntegral (length labels)
        classCounts = map (\(x:xs) -> (x, fromIntegral (1 + length xs) / total)) (group (sort labels))
        sumSquares = sum $ map (\(_, p) -> p * p) classCounts
    in 1 - sumSquares

-- Compute Gini impurity for a given split
giniSplit :: [(Double, String)] -> Double -> Double
giniSplit sortedPairs threshold =
    let (left, right) = span (\(val, _) -> val <= threshold) sortedPairs
        leftLabels = map snd left
        rightLabels = map snd right
        total = fromIntegral (length sortedPairs)
        leftWeight = fromIntegral (length left) / total
        rightWeight = fromIntegral (length right) / total
    in leftWeight * giniIndex leftLabels + rightWeight * giniIndex rightLabels


-- Find best threshold for the feature based on gini index of the split
findBestThresholdAndGini :: [Double] -> [String] -> (Double, Double)
findBestThresholdAndGini feature labels =
    let sortedPairs = sort $ zip feature labels
        possibleThresholds = [ (x + y) / 2
                             | ((x, _), (y, _)) <- zip sortedPairs (drop 1 sortedPairs)]
        thresholdGinis = [ (t, giniSplit sortedPairs t) | t <- possibleThresholds ]
    in if null thresholdGinis
        then case feature of
         []    -> (0.0, 1.0) 
         (x:_) -> (x, 1.0)
        else minimumBy (comparing snd) thresholdGinis

-- Split data based on the threshold
splitData :: Int -> [[Double]] -> [String] -> Double -> ([[Double]], [String], [[Double]], [String])
splitData featureIndex features labels threshold =
    let selectedFeature = features !! featureIndex
        featureLabelPairs = zip selectedFeature (zip (transpose features) labels)
        (leftPairs, rightPairs) = partition (\(val, _) -> val <= threshold) featureLabelPairs
        (leftFeaturesWithLabels, leftLabels) = unzip $ map snd leftPairs
        (rightFeaturesWithLabels, rightLabels) = unzip $ map snd rightPairs
    in (transpose leftFeaturesWithLabels, leftLabels, transpose rightFeaturesWithLabels, rightLabels)

-- Find the best split
findBestSplit :: [[Double]] -> [String] -> (Int, Double, Double)
findBestSplit features labels =
    let indexedFeatures = zip [0..] features
        results = [ (i, threshold, gini)
                  | (i, feature) <- indexedFeatures
                  , let (threshold, gini) = findBestThresholdAndGini feature labels ]
    in minimumBy (comparing (\(_, _, g) -> g)) results


-- Create decision tree
createTree :: [[Double]] -> [String] -> Tree Int Double
createTree _ [] = EmptyTree
createTree features (label:restLabels)
    | all (== label) restLabels = Leaf label
    | otherwise =
        let (bestIndex, bestThreshold, _) = findBestSplit features (label : restLabels)
            (leftFeatures, leftLabels, rightFeatures, rightLabels) =
                splitData bestIndex features (label : restLabels) bestThreshold
            leftSubtree = createTree leftFeatures leftLabels
            rightSubtree = createTree rightFeatures rightLabels
        in Node bestIndex bestThreshold leftSubtree rightSubtree


-- Print of spaces to ensure the right hierarchy
printSpaces :: Int -> String
printSpaces n = replicate (n * 2) ' ' 

-- Print the decision tree 
printTree :: (Show i, Show t) => Tree i t -> Int -> String
printTree EmptyTree _ = ""  -- Empty tree returns an empty string
printTree (Node index threshold left right) num =
    -- Print the current node, followed by its children (left and right)
    printSpaces num ++ "Node: " ++ show index ++ ", " ++ show threshold ++ "\n" ++
    printTree left (num + 1) ++
    printTree right (num + 1)
printTree (Leaf classString) num =
    -- Print the leaf with appropriate indentation
    printSpaces num ++ "Leaf: " ++ classString ++ "\n"

-- Remove the last newline symbol
removeLast :: String -> String
removeLast str =
    if last str == '\n' then init str else str



main :: IO()
main = do 
    args <- getArgs
    case args of 
        ["-1", inputTree, inputData] -> do
            loadedTree <- readFile inputTree
            loadedData <- readFile inputData
            --print (lines loadedData)
            let processedData = convert (lines loadedData) 
            --print processedData
            
            --print (lines loadedTree)
            
            let treeLines = lines loadedTree
            let parsedLines = map parseLine treeLines 
            --print parsedLines
            
            let (builtTree, _) = buildTree parsedLines 
            --print builtTree  -- Print the resulting tree

            let result = classifyAll builtTree processedData
            mapM_ putStrLn result

        ["-2", inputTrainingData] -> do
            loadedData <- readFile inputTrainingData

            let (features, labels) = processData (lines loadedData)
            --print features
            --print labels
            
            let decisionTree = createTree features labels   
            --print decisionTree

            let result = printTree decisionTree 0
            putStrLn (removeLast result)


        _ -> putStrLn "Incorrect arguments"
