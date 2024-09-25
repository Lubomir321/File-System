{-# LANGUAGE LambdaCase #-}
import System.Exit (exitSuccess)
import Data.Maybe 
import System.IO
import Control.Monad.State.Class (MonadState(get))
--import Control.Monad.Cont (cont)


-- File and Directory data types

data File = File { fileName :: String, fileContent :: String } deriving (Show)
data Directory = Directory { dirName :: String, contents :: [Maybe FileSystem], parent :: Maybe FileSystem } deriving (Show)
data FileSystem = FSFile File | FSDirectory Directory deriving (Show)


-- Function to get the name of a FileSystem entity
getName :: [Maybe FileSystem] -> String
getName [] = ""  -- Handle the case of an empty list
getName (Just (FSFile (File name _)):xs) = name ++ " " ++ getName xs
getName (Just (FSDirectory (Directory name _ _)):xs) = name ++ " " ++ getName xs 
getName (Nothing:xs) = ""  -- Handle the case of a Nothing value


--Function to get the parent of Directory
getParent ::  Directory -> Maybe FileSystem
getParent (Directory _ _ p) = p

--Structure of file system where the simulation will be
file1 :: File
file1 = File { fileName = "file1.txt", fileContent = "Aaaaa" } 

file2 :: File
file2 = File { fileName = "file2.txt", fileContent = "BBBBBBBBBB" }

file3 :: File
file3 = File { fileName = "file3.txt", fileContent = "VVVVVVVV" }

file4 :: File
file4 = File { fileName = "file4.txt", fileContent = "432432432" }

root :: Directory
root = Directory { dirName = "/", contents = [], parent = Nothing }

dir1 :: Directory
dir1 = Directory { dirName = "dir1", contents = [], parent = Just (FSDirectory root) }

dir2 :: Directory
dir2 = Directory { dirName = "dir2", contents = [], parent = Just (FSDirectory root) }

dir3 :: Directory
dir3 = Directory { dirName = "dir3", contents = [], parent = Just (FSDirectory dir2) }

dir4 :: Directory
dir4 = Directory { dirName = "dir4", contents = [], parent = Just (FSDirectory dir3) }

--If i didnt separate the contents and put them in the [] of dir1 dir2 etc an infinite recursion occurs, because if i want to see
-- lets say dir2 it will print it and also it will print the parent content and there will be dir2 again and this will occur forever
rootWithContent :: Directory
rootWithContent = root { contents = [Just (FSDirectory dir1),Just (FSDirectory dir2)] }

dir2WithContent :: Directory
dir2WithContent = dir2 { contents = [Just (FSFile file3),Just (FSDirectory dir3)] }

dir1WithContent :: Directory
dir1WithContent = dir1 { contents = [Just (FSFile file1), Just (FSFile file2)] }

dir3WithContent :: Directory
dir3WithContent = dir3 { contents = [Just (FSFile file4), Just (FSDirectory dir4)] }

fileSystem1 :: FileSystem
fileSystem1 = FSDirectory rootWithContent

--A function to that seprates a given string by a given char into an array
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim str =
    let (before, after) = break (== delim) str
    in case after of
        [] -> [before]
        _ -> before : splitOn delim (tail after)

--Splits the path for example "/a/b/c" = ["a", "b", "c"]
splitPath :: String -> [String]
splitPath = filter (not . null) . splitOn '/' 

--Splits the input for command for example "cd /a/b/c" = ["cd","a/b/c"]
splitInput :: String -> [String]
splitInput = filter (not . null) . splitOn ' '

--List of available commands
validCommands :: [String]
validCommands = ["ls", "pwd", "cat", "cd", "rm"]

--Checks if a commad is valid
isValidCommand :: [String] -> String -> Bool
isValidCommand [] _ = False
isValidCommand (x:xs) cmd = (x == cmd) || isValidCommand xs cmd

--Checks if a command exists
existCommand :: String -> [String] -> Bool
existCommand _ [] = False
existCommand com (x:xs) = (com == x) || existCommand com xs

getLast :: [a] -> a
getLast xs = last xs

getAllButLast :: [a] -> [a]
getAllButLast xs = init xs

--currDirectory :: Directory
--currDirectory = dir1WithContent
--rootDirecotry :: Directory
--rootDirecotry = rootWithContent

-- The main function
main :: IO ()
main = do
    let rootDir = rootWithContent  -- Assuming rootWithContent is a Directory
    mainLoop rootDir               -- Start the command loop with the root directory

-- The main loop that keeps asking for commands and processing them
mainLoop :: Directory -> IO ()
mainLoop currDirectory = do
    putStr "$ "
    userInput <- getLine
    newDir <- processCommand userInput currDirectory  -- Process the command and get the updated directory
    mainLoop newDir  -- Pass the updated directory back into the loop

-- The function that processes each command
processCommand :: String -> Directory -> IO Directory
processCommand "exit" _ = do
    putStrLn "Exiting..."
    exitSuccess
processCommand command currDirectory = do
    let input = splitInput command
    case head input of
        "ls" -> do
            if null (tail input)
                then listContents currDirectory
                else listContentsPath rootWithContent (splitPath (concat (tail input)))
            return currDirectory  -- No directory change

        "pwd" -> do
            pwd (FSDirectory currDirectory)
            return currDirectory  -- No directory change

        "cd" -> do
            let newDir = head (tail input)
            if isPath newDir
                then do
                    if isFullPath newDir
                        then do
                            updatedDir <- cdPath currDirectory (extractDirContent rootWithContent) (splitPath newDir)
                            processCommand "pwd" updatedDir
                            return updatedDir
                        else do
                            updatedDir <- cdPath currDirectory (extractDirContent currDirectory) (splitPath newDir)
                            processCommand "pwd" updatedDir
                            return updatedDir
                else do
                    updatedDir <- processCd currDirectory newDir
                    processCommand "pwd" updatedDir
                    return updatedDir

        "cat" -> do
            if length (tail input) > 1
                then do
                    if filesExist currDirectory (getAllButLast (tail input))
                        then do
                            updatedDir <- cat currDirectory (getAllButLast (tail input)) (getLast (tail input))
                            processCommand "pwd" updatedDir
                            return updatedDir
                        else do
                             putStrLn "Invalid cat command. Usage: cat newFileName file1 file2 ..."
                             return currDirectory
                else do
                    if length (tail input) == 1 && filesExist currDirectory (tail input)
                        then do
                            updatedDir <- cat currDirectory (tail input) (getLast (tail input))
                            processCommand "pwd" updatedDir
                            return updatedDir
                        else do
                             putStrLn "Invalid cat command. Usage: cat newFileName file1 file2 ..."
                             return currDirectory
            return currDirectory  -- No directory change if command was invalid

        "rm" -> do
            if filesExist currDirectory (tail input)
                then do
                    updatedDir <- removeAll currDirectory (tail input)
                    --let finalDir = propagateChangesUp updatedDir currDirectory --does not update properly 
                    processCommand "pwd" updatedDir
                    return updatedDir
                else do
                    putStrLn "Invalid files"
                    return currDirectory  -- No directory change

        "save" -> case tail input of
            [filePath] -> do
                saveFileSystemToFile filePath (FSDirectory currDirectory)
                processCommand "pwd" currDirectory
                return currDirectory  -- Return the current directory after saving
            _ -> do
                putStrLn "Invalid save command. Usage: save filePath"
                return currDirectory  -- Return the current directory if the command is invalid

filesExist :: Directory -> [String] -> Bool
filesExist dir lst = checkExistence (extractDirContent dir) lst
    where
        checkExistence :: [Maybe FileSystem] -> [String] -> Bool
        checkExistence [] _ = False
        checkExistence _ [] = True
        checkExistence (Just (FSFile (File name content)): xs) (y:ys) = if name == y then checkExistence (extractDirContent dir) ys else checkExistence xs (y:ys)
        checkExistence (Just (FSDirectory (Directory {})) : xs) (y:ys) = checkExistence xs (y:ys)




isFullPath :: String -> Bool
isFullPath str = isValidPath (extractDirContent rootWithContent) (splitPath str)

isPath :: String -> Bool
isPath str = '/' `elem` str

-- Function to list contents of a directory
listContents :: Directory -> IO ()
listContents dir = do
    putStrLn $ "Listing contents of " ++ dirName dir
    if null (contents dir)
        then putStrLn "Empty directory"
        else mapM_ printFileOrDir (catMaybes $ contents dir)


listContentsPath :: Directory -> [String] -> IO ()
listContentsPath dir lst = do
    let validPath = isValidPath (extractDirContent rootWithContent) lst 
    if validPath  then print ( getName (getDirContent avlbDirs (last lst))) else putStrLn "Invalid path"

-- Function to print a file or directory
printFileOrDir :: FileSystem -> IO ()
printFileOrDir (FSFile file) = putStrLn $ "File: " ++ fileName file
printFileOrDir (FSDirectory dir) = putStrLn $ "Directory: " ++ dirName dir
-- Function to print the current working directory
pwd :: FileSystem -> IO ()
pwd dir = do
    putStrLn $ "Current directory: " ++ removeConsecutiveSlashes (constructPath dir)

--This function removes consecutvie slashes exmpale "//" = "/"
--if pwd is run without it it prints a slash for the root and also when appending so it is like that //dir1
removeConsecutiveSlashes :: String -> String
removeConsecutiveSlashes = go
  where
    go [] = []
    go ('/':xs@(y:_)) | y == '/' = go xs
    go (x:xs) = x : go xs


-- Function to construct the path by traversing the parent chain
constructPath :: FileSystem -> String
constructPath (FSDirectory dir) = case parent dir of
    Nothing -> dirName dir
    Just parentDir -> constructPath parentDir ++ "/" ++ dirName dir
-- Function to change the current working directory


-- Function to change the current working directory
processCd :: Directory -> String -> IO Directory
processCd currDir targetDir  = do
        newDir <- cd currDir targetDir
        putStrLn $ "Changing directory to: " ++ dirName newDir
        return newDir



-- Function to change the current working directory
cd :: Directory -> String -> IO Directory
cd currDir targetDir
    | targetDir == ".." = case parent currDir of
        Just (FSDirectory parentDir) -> return (getDirForIO avlbDirs currDir (dirName parentDir))
        Nothing -> return currDir
    | otherwise = case findSubdirectory currDir targetDir of
        Just newDir -> return newDir
        Nothing -> putStrLn ("Directory not found: " ++ targetDir) >> return currDir -- Handle directory not found


findSubdirectory :: Directory -> String -> Maybe Directory
findSubdirectory currDir targetDir = getDir avlbDirs currDir targetDir
    


-- Function to add a file to the current directory
addFileToDirectory :: Directory -> File -> Directory
addFileToDirectory currDir newFile =
    currDir { contents = contents currDir ++ [Just (FSFile newFile)] }


getFilesContent :: Directory -> [String] -> String
getFilesContent (Directory dirName content _ ) files = smth content files
    where 
        smth :: [Maybe FileSystem] -> [String] -> String
        smth [] _ = ""
        smth _ [] = ""
        smth (Just (FSFile (File name fContent)) : ys) (x:xs) = if name == x then fContent ++ smth content xs else smth ys (x:xs)
        smth (Just (FSDirectory {}):ys) lst = smth ys lst

cat :: Directory -> [String] -> String -> IO Directory
cat currDir fileNames newFileName = do
    let numArgs = length fileNames
    print (show numArgs)
    case numArgs of
        0 -> do
            putStrLn "Invalid Files"
            return currDir
        1 -> do
            let fileName = head fileNames
            printFileContent currDir fileName
            return currDir
        _ -> do
            let content = getFilesContent currDir fileNames
            let newFile = File { fileName = newFileName, fileContent = content }
            let newDir = addFileToDirectory currDir newFile
            putStrLn $ "Concatenated contents into " ++ newFileName
            return newDir



getFileContent :: Directory -> String -> String
getFileContent (Directory _ content _) fName = opa content fName
    where
        opa :: [Maybe FileSystem] -> String -> String
        opa [] _ = "Didnt find the file!"
        opa (Just (FSFile (File name fContent)) : xs) fName = if name == fName then fContent else opa xs fName


-- Function to print the content of a single file
printFileContent :: Directory -> String -> IO ()
printFileContent dir fileName = do
        putStrLn $ "File content " ++ getFileContent dir fileName

removeAll :: Directory -> [String] -> IO Directory
removeAll curDir [] = return curDir  -- If no file/directory names are passed, return the current directory
removeAll curDir (x:xs) = do
    -- Filter out only the file or directory with the given name 'x'
    let newContents = filter (\case
            Just (FSFile (File name _)) -> x /= name  -- Remove file if it matches
            Just (FSDirectory (Directory dirName _ _)) -> x /= dirName  -- Remove directory if it matches
            _ -> True  -- Keep everything else
            ) (contents curDir)
    
    -- Recursively remove remaining files/directories
    if null xs
        then return curDir { contents = newContents }  -- No more files/directories to remove, return updated directory
        else removeAll curDir { contents = newContents } xs

-- Recursively updates the parent directory with the modified child directory
updateParentWithChild :: Directory -> Directory -> Directory
updateParentWithChild parentDir updatedChildDir =
    let updatedContents = map (\case
            Just (FSDirectory dir) | dirName dir == dirName updatedChildDir -> Just (FSDirectory updatedChildDir)
            other -> other
            ) (contents parentDir)
    in parentDir { contents = updatedContents }

-- This function takes the updated directory and the current directory and propagates the changes up to the root.
propagateChangesUp :: Directory -> Directory -> Directory
propagateChangesUp updatedDir curDir =
    case parent curDir of
        Just (FSDirectory parentDir) -> 
            -- Replace the current directory in the parent's contents
            let updatedParentContents = map (updateChild updatedDir) (contents parentDir)
                updatedParent = parentDir { contents = updatedParentContents }
            in propagateChangesUp updatedParent parentDir  -- Recursively propagate changes upwards
        Nothing -> updatedDir  -- If we reach the root, return the updated directory

-- Helper function to update a child directory or file in the parent directory's contents
updateChild :: Directory -> Maybe FileSystem -> Maybe FileSystem
updateChild updatedDir (Just (FSDirectory dir))
    | dirName dir == dirName updatedDir = Just (FSDirectory updatedDir)  -- Replace the old directory with the updated one
updateChild _ fs = fs  -- Keep all other files/directories unchanged



--This is an array of available directories with content
avlbDirs :: [Directory]
avlbDirs = [dir1WithContent, dir2WithContent, dir3WithContent, rootWithContent]

--This gives the content of a particular directory search by given name
getDirContent :: [Directory] -> String -> [Maybe FileSystem]
getDirContent [] _ = [Nothing]
getDirContent ((Directory dirName content _) : xs) searchedName = if dirName == searchedName then content else getDirContent xs searchedName

getDir :: [Directory] -> Directory -> String -> Maybe Directory
getDir [] curdir _ = Just curdir
getDir ((Directory dirName contents parent) : xs) curdir searchedName = if dirName == searchedName then Just (Directory dirName contents parent) else getDir xs curdir searchedName

getDirForIO :: [Directory] -> Directory -> String -> Directory
getDirForIO [] curdir _ = curdir
getDirForIO ((Directory dirName contents parent) : xs) curdir searchedName = if dirName == searchedName then Directory dirName contents parent else getDirForIO xs curdir searchedName


--This checks if a fullPath is valid by traversing from the root content to the last met
isValidPath :: [Maybe FileSystem] -> [String] -> Bool
isValidPath _ [] = True
isValidPath [] _ = False
isValidPath (Just (FSDirectory (Directory dirName _ _)):xs) (y:ys) = if dirName == y then isValidPath (getDirContent avlbDirs dirName) ys else isValidPath xs (y:ys)
isValidPath (Just (FSFile (File _ _ )) : xs) lst = isValidPath xs lst


--This function gives the content of a passed direcotry
extractDirContent :: Directory -> [Maybe FileSystem]
extractDirContent (Directory _ content _) = content


--Implementation of full path for cd
cdPath :: Directory -> [Maybe FileSystem] -> [String] -> IO Directory
cdPath curDir [] _ = putStrLn "Empty list of directories." >> return curDir 
cdPath curDir (Just (FSDirectory (Directory dirName contents parent)) : xs) (y:ys)
    | dirName == y && null ys = return (getDirForIO avlbDirs curDir y) 
    | dirName == y = cdPath curDir (getDirContent avlbDirs dirName) ys
    | otherwise = cdPath curDir xs (y:ys)
cdPath curDir (Just (FSFile (File {})) : xs) lst = cdPath curDir xs lst


--Saving to file the current state of the file system
saveFileSystemToFile :: FilePath -> FileSystem -> IO ()
saveFileSystemToFile filePath fileSystem = do
    let fileSystemString = show fileSystem
    writeFile filePath fileSystemString
    putStrLn $ "FileSystem saved to: " ++ filePath


