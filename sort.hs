-- quick sort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) =
  quickSort l ++ [x] ++ quickSort r
  where l = [a | a <- xs, a <= x]
        r = [a | a <- xs, a > x]

-- selection sort
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]
selectionSort xs =
  min:selectionSort [x | x <- xs, x /= min]
  where min = minimum xs

-- bubble sort
bubble :: (Ord a) => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs)
  | x < y = x:bubble (y:xs)
  | otherwise = y:bubble (x:xs)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort xs = bubbleSort (init r) ++ [last r]
  where r = bubble xs

-- marge sort
marge :: (Ord a) => [a] -> [a] -> [a]
marge xs [] = xs
marge [] ys = ys
marge (x:xs) (y:ys)
  | x < y = x:marge xs (y:ys)
  | otherwise = y:marge (x:xs) ys

margeSort :: (Ord a) => [a] -> [a]
margeSort [] = []
margeSort [x] = [x]
margeSort xs = marge (margeSort l) (margeSort r)
  where l = take half xs
        r = drop half xs
        half = length xs `div` 2

main = do
  let xs = [9,8,7,6,5,4,3,2,1,0]

  print $ "Before         : " ++ show xs
  print $ "Quick Sort     : " ++ show (quickSort xs)
  print $ "Selection Sort : " ++ show (selectionSort xs)
  print $ "Bubble Sort    : " ++ show (bubbleSort xs)
  print $ "Marge Sort     : " ++ show (margeSort xs)
  