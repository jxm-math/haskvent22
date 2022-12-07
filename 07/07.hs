import Data.List (isPrefixOf)

type Name = String
type Size = Int

-- file system model
data FSItem = File Name Size | Folder Name [FSItem]

-- file system traversal utilities
data FSCrumb = FSCrumb Name [FSItem] [FSItem]
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsRoot :: FSZipper -> FSZipper
fsRoot (item, []) = (item, [])
fsRoot z = fsRoot (fsUp z)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)
    where
        nameIs name (Folder folderName _) = name == folderName
        nameIs name (File fileName _) = name == fileName

fsNewItem :: FSItem -> FSZipper -> FSZipper
fsNewItem item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)

-- bash decoding
decode :: String -> FSZipper -> FSZipper
decode "$ ls" = id
decode "$ cd /"  = fsRoot
decode "$ cd .." = fsUp
decode s | "$ cd " `isPrefixOf` s = fsTo (drop 5 s)
decode s | "dir "  `isPrefixOf` s = fsNewItem (Folder (drop 4 s) [])
decode s = let [size, name] = words s in fsNewItem (File name (read size))

-- size utilities
totalSize :: FSItem -> Int
totalSize (File _ size) = size
totalSize (Folder _ items) = sum $ map totalSize items

sumOfSmallDirs :: FSItem -> Int
sumOfSmallDirs (File _ _) = 0
sumOfSmallDirs f@(Folder _ items) =
    let s = totalSize f
    in (if s <= 100000 then s else 0) + sum (map sumOfSmallDirs items)

findSmallerDirUpon :: Int -> FSItem -> Int
findSmallerDirUpon _ (File _ _) = 0
findSmallerDirUpon k f@(Folder _ items) =
    let sizes = filter (/= 0) $ map (findSmallerDirUpon k) items
    in if not $ null sizes
        then minimum sizes
        else let s = totalSize f in if s >= k then s else 0

-- empty filesystem
initZipper = (Folder "/" [], [])

main :: IO ()
main = do
    input <- readFile "./input.txt"
    let orders = lines input
    let finalZipper = foldl (flip decode) initZipper orders
    let fs = fst $ fsRoot finalZipper

    putStrLn "first answer"
    print $ sumOfSmallDirs fs

    putStrLn "second answer"
    print $ findSmallerDirUpon (totalSize fs - 40000000) fs