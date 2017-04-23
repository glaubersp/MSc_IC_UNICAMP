module GenSort where
import Data.List

data Split a b = Split b [[a]]

genSort :: Ord a => ([a] -> Split a b) -> (Split a b -> [a]) -> [a] -> [a]
genSort split join l = case l of
  _ : _ : _ -> let Split c ls = split l in
               join $ Split c $ map (genSort split join) ls
  _ -> l

splitInsertionSort :: [a] -> Split a a
splitInsertionSort (a : l) = Split a [l]

joinInsertionSort :: Ord a => Split a a -> [a]
joinInsertionSort (Split a [l]) = insert a l

insertionSort :: Ord a => [a] -> [a]
insertionSort = genSort splitInsertionSort joinInsertionSort

splitQuickSort :: Ord a => [a] -> Split a a
splitQuickSort (a : l) =
  let (ls, gs) = partition (< a) l in Split a [ls, gs]

joinQuickSort :: Split a a -> [a]
joinQuickSort (Split a [ls, gs]) = ls ++ (a : gs)

quickSort :: Ord a => [a] -> [a]
quickSort = genSort splitQuickSort joinQuickSort

splitMergeSort :: [a] -> Split a ()
splitMergeSort l =
  let (l1, l2) = splitAt (div (length l) 2) l in Split () [l1, l2]

joinMergeSort :: Ord a => Split a () -> [a]
joinMergeSort (Split _ [l1, l2]) = merge l1 l2

merge :: Ord a => [a] -> [a] -> [a]
merge l1 l2 = case l1 of
  [] -> l2
  x1 : r1 -> case l2 of
     [] -> l1
     x2 : r2 -> if x1 < x2
       then x1 : merge r1 l2
       else x2 : merge l1 r2

mergeSort :: Ord a => [a] -> [a]
mergeSort = genSort splitMergeSort joinMergeSort

splitSelectionSort :: Ord a => [a] -> Split a a
splitSelectionSort l =
  let m = minimum l in Split m [delete m l]

joinSelectionSort :: Split a a -> [a]
joinSelectionSort (Split a [l]) = a : l

selectionSort :: Ord a => [a] -> [a]
selectionSort = genSort splitSelectionSort joinSelectionSort

prop_sort1 :: [Int] -> Bool
prop_sort1 l = insertionSort l == quickSort l

prop_sort2 :: [Int] -> Bool
prop_sort2 l = mergeSort l == quickSort l

prop_sort3 :: [Int] -> Bool
prop_sort3 l = mergeSort l == selectionSort l

-- | The 'partition' function takes a predicate a list and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p xs == (filter p xs, filter (not . p) xs)

ppartition :: (a -> Bool) -> [a] -> ([a],[a])
ppartition p xs = foldr (select p) ([],[]) xs

select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)