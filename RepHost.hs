import System.Directory (doesFileExist)
import System.Posix.Signals (Handler(..), installHandler, keyboardSignal)
import System.Hardware.Serialport
import Data.Char (isAlpha, ord, isSpace)
import Data.List (isPrefixOf)
import Control.Monad (liftM, forM_, when)
import Control.Monad.Trans (lift)
import Data.Maybe (mapMaybe)
import Text.Printf
import qualified Data.Text as T
import System.Environment (getArgs)
import Data.Bits
import System.Console.Haskeline
import Data.List.Split (splitWhen)

port = "/dev/ttyUSB0"
settings = defaultSerialSettings { commSpeed=CS115200
                                 , flowControl=Software 
                                 , timeout=500 }

data Reply = Temperature [(Char,Double)]
           | OK String
           | Resend Integer
           | Fault
           | Error String
           | Unknown String
           deriving (Show, Eq)

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

type Cmd = T.Text
filterCmd :: Cmd -> Maybe Cmd
filterCmd c
        | T.null c = Nothing
        | not $ isAlpha (T.head c) = Nothing
        | otherwise = Just $ T.strip $ T.takeWhile (/=';') c

printFile :: SerialPort -> FilePath -> IO ()
printFile port file = 
        do cmds <- liftM (T.lines . T.pack) $ readFile file
           sendCmds port cmds

checksum :: Cmd -> Int
checksum s = 0xff .&. T.foldl' (\s c->s `xor` ord c) 0 s

appendChecksum :: Cmd -> Cmd
appendChecksum c = c `T.snoc` '*' `T.append` T.pack (show $ checksum c)

recvLine :: SerialPort -> IO String
recvLine port = 
        do c <- recvChar port
           case c of
                Nothing   -> error "Response timed out"
                Just '\n' -> return []
                Just c    -> do rest <- recvLine port
                                return (c:rest)

recvReply :: SerialPort -> IO Reply
recvReply port =
        do reply <- recvLine port
           case () of
                _ | "start" `isPrefixOf` reply -> recvReply port
                _ | "//" `isPrefixOf` reply    -> recvReply port
                _ | "rs" `isPrefixOf` reply    -> return $ Resend (read $ drop 3 reply)
                _ | "!!" `isPrefixOf` reply    -> return $ Fault
                _ | "ok" `isPrefixOf` reply    -> return $ OK $ drop 3 reply
                otherwise                      -> return $ Unknown reply

sendCmds :: SerialPort -> [Cmd] -> IO ()
sendCmds port cmds =
       let cmds' = mapMaybe filterCmd cmds
           ncmds = length cmds'
       in forM_ (zip [1..] cmds') $ \(n,c) ->
               do printf "%4d / %4d :  %s\n" (n::Integer) ncmds (T.unpack $ T.strip c)
                  case filterCmd c of
                       Just cmd -> do sendCmd port c
                                      return ()
                       Nothing -> return ()
          
sendCmd :: SerialPort -> Cmd -> IO Reply
sendCmd port cmd =
        let cmd' = appendChecksum cmd
        in do sendString port (T.unpack cmd)
              sendChar port '\n'
              reply <- recvReply port
              return reply

magicCmd :: SerialPort -> String -> InputT IO ()
magicCmd port cmd
        | "print" `isPrefixOf` cmd =
                let path = strip $ drop 6 cmd
                in do exists <- lift $ doesFileExist path
                      if exists then lift $ printFile port path
                                else outputStrLn "File doesn't exist"

cmdLoop :: SerialPort -> IO ()
cmdLoop port = runInputT defaultSettings loop
        where loop :: InputT IO ()
              loop = do
                      minput <- getInputLine "> "
                      case minput of
                           Nothing         -> return ()
                           Just "exit"     -> return ()
                           Just "%exit"    -> return ()
                           Just "quit"     -> return ()
                           Just "%quit"    -> return ()
                           Just ('%':cmd)  -> do magicCmd port cmd
                                                 loop
                           Just input      -> do reply <- lift $ sendCmd port (T.pack input)
                                                 outputStrLn $ show reply
                                                 loop

main = withSerial port settings f
        where f port =
                do installHandler keyboardSignal (Catch (cmdLoop port)) Nothing
                   cmdLoop port

