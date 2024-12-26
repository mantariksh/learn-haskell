import Data.Char
import Data.List
import System.IO

type TodoList = [String]

type TodoNumber = Int

numberTodos :: TodoList -> [String]
numberTodos = zipWith (\idx todo -> show idx ++ ". " ++ todo) [1 ..]

validateNumToDelete :: String -> Int -> Either String Int
validateNumToDelete rawNum numTodos
  | all isDigit rawNum && isValidTodo todoNum = Right todoNum
  | all isDigit rawNum = Left "Todo number out of bounds"
  | otherwise = Left "Invalid number!"
  where
    todoNum = read rawNum :: Int
    isValidTodo todoNum = todoNum > 0 && todoNum <= numTodos

removeByIdx :: Int -> [a] -> [a]
removeByIdx idx l = take idx l ++ drop (idx + 1) l

writeNewTodoList :: TodoList -> IO ()
writeNewTodoList = writeFile "todos.txt" . intercalate "\n"

deleteTodo :: TodoNumber -> TodoList -> IO ()
deleteTodo todoNum = writeNewTodoList . removeByIdx (todoNum - 1)

main = do
  currentTodosContents <- readFile "todos.txt"
  let currentTodos = lines currentTodosContents
      numTodos = length currentTodos
  putStrLn "Here are your current todos:"
  mapM_ putStrLn $ numberTodos currentTodos
  putStrLn ""
  putStrLn "Which number would you like to delete?"
  numToDeleteRaw <- getLine
  case validateNumToDelete numToDeleteRaw numTodos of
    Left err -> putStrLn err
    Right todoNum -> do
      deleteTodo todoNum currentTodos
      putStrLn $ "Deleted todo: " ++ show todoNum
