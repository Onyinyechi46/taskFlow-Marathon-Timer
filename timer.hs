module Main where

import Control.Monad
import Data.Bits (shift)
import Data.IORef
import Data.Int (Int16)
import Data.List.Split (splitOn)
import Data.Time
import Graphics.Win32
import System.IO
import System.IO.Error (catchIOError)
import System.Locale (defaultTimeLocale)
import System.Win32.DLL (getModuleHandle)
import System.Win32.Types

import Font
import GradientFill2
import Sound
import TheTime

foreign import ccall "PostQuitMessage" postQuitMessage :: Int -> IO ()

-- Application configuration
configFilePath :: String
configFilePath = "config"

defaultConfig :: (Integer, Integer, (Int, Int))
defaultConfig = (3 * 60, 80, (250, 250))

colorTheme :: [COLORREF]
colorTheme = [rgb 255 255 255, rgb 255 145 15, rgb 241 129 1]

-- Application state

data AppState
  = Idle
  | CountingDown UTCTime
  | Alarming Int

-- Entry Point
main :: IO ()
main = allocaPAINTSTRUCT $ \lpps -> do
  registerMyClass
  hwnd <- createMyWindow lpps
  sendMessage hwnd wM_USER 0 0
  showWindow hwnd sW_SHOWNORMAL
  setWinTimer hwnd 42 100
  allocaMessage pump
  unregisterMyClass

-- Window registration and creation
className :: LPCTSTR
className = mkClassName "CountdownTimer"

registerMyClass :: IO (Maybe ATOM)
registerMyClass = do
  hinst <- getModuleHandle Nothing
  hcursor <- loadCursor Nothing iDC_ARROW
  whiteBrush <- getStockBrush wHITE_BRUSH
  registerClass
    (cS_HREDRAW + cS_VREDRAW, hinst, Nothing, Just hcursor, Just whiteBrush, Nothing, className)

unregisterMyClass :: IO ()
unregisterMyClass = do
  hinst <- getModuleHandle Nothing
  unregisterClass className hinst

createMyWindow :: LPPAINTSTRUCT -> IO HWND
createMyWindow lpps = do
  hinst <- getModuleHandle Nothing
  windowProc <- makeWindowProc lpps
  createWindowEx
    0x02000000
    className
    "Countdown Timer"
    (wS_CAPTION + wS_MINIMIZEBOX + wS_SYSMENU)
    Nothing Nothing Nothing Nothing
    Nothing Nothing hinst windowProc

-- Main message loop
pump :: LPMSG -> IO ()
pump lpmsg = do
  fContinue <- getMessage lpmsg Nothing
  when fContinue $ do
    translateMessage lpmsg
    dispatchMessage lpmsg
    pump lpmsg

-- Timer string formatting

dummyDay :: Day
dummyDay = fromGregorian 2000 1 1

showDiffTime :: DiffTime -> String
showDiffTime = formatTime defaultTimeLocale "%H:%M:%S" . UTCTime dummyDay

showZonedTime :: UTCTime -> IO String
showZonedTime time = do
  zonedTime <- utcToLocalZonedTime time
  return $ formatTime defaultTimeLocale "%H:%M:%S" zonedTime

-- Additional utilities
removeComment :: String -> String
removeComment = head . splitOn "--"

saveConfig :: (Integer, Integer, (Int, Int)) -> IO ()
saveConfig (t, h, loc) = writeFile configFilePath $ unlines
  [ show t ++ " -- timeValue"
  , show h ++ " -- fontHeight"
  , show loc ++ " -- windowLocation"
  ]

loadConfig :: (Integer, Integer, (Int, Int)) -> IO (Integer, Integer, (Int, Int))
loadConfig def = flip catchIOError (\_ -> return def) $ do
  content <- readFile configFilePath
  let [t, h, loc] = map (read . removeComment) (lines content)
  return (t, h, loc)
