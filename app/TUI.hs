{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use foldM_" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TUI where

import Control.Monad (foldM, mzero)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Graphics.Vty
  ( Attr,
    Background (Background),
    DisplayRegion,
    Event (EvKey, EvResize),
    Image,
    Key (KBS, KChar, KDown, KEnter, KEsc, KLeft, KRight, KUp),
    Modifier (MCtrl),
    Output (displayBounds),
    Picture (picBackground, picLayers),
    Vty (nextEvent, outputIface, shutdown, update),
    brightBlue,
    charFill,
    defAttr,
    emptyPicture,
    horizCat,
    mkVty,
    standardIOConfig,
    standout,
    string,
    translate,
    translateY,
    underline,
    vertCat,
    white,
    withBackColor,
    withForeColor,
    withStyle,
    (<->),
    (<|>),
  )
import Graphics.Vty.Image (char)
import Model (Address, Enviroment (cells), Expression (sourceRange), LiteralValue (Null, String), addressAsString, emptyEnviroment, evaluate, expressionParser, mainParser, numToLetters)
import Text.Parsec (ParseError, parse)

type Sheet = Map.Map Address String

data State = State
  { bounds :: DisplayRegion,
    selected :: Address,
    scroll :: Address,
    sheet :: Sheet,
    editing :: Bool,
    editor :: Editor
  }

data Editor = Editor
  { before :: String,
    after :: String
  }

instance Show Editor where
  show Editor {before, after} = before ++ after

runTUI :: IO ()
runTUI = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  bounds <- displayBounds $ outputIface vty
  let state = (State {bounds, selected = (3, 5), scroll = (0, 0), editing = False, editor = Editor {before = "", after = ""}, sheet = mempty})
  update vty $ drawScreen state
  loop vty state
  shutdown vty
  where
    loop vty state = do
      event <- nextEvent vty
      let nstate = handleEvent event state
      case event of
        EvKey (KChar 'c') [MCtrl] -> putStrLn "quiting"
        x -> update vty (drawScreen nstate) >> loop vty nstate

handleEvent :: Event -> State -> State
handleEvent (EvResize cols rows) state = state {bounds = (cols, rows)}
handleEvent (EvKey KEnter []) state@State {editing = False} = state {editing = True}
handleEvent (EvKey k []) state@State {editing = True} = handleEditing k state
handleEvent (EvKey k []) state@State {editing = False} = handleMove k state
-- \| k `elem` [KDown, KUp, KLeft, KRight]
handleEvent event state = state

handleEditing :: Key -> State -> State
handleEditing k state@State {editor = editor@Editor {before, after}, sheet, selected} = case k of
  KEsc -> state {editing = False, sheet = if null (show editor) then Map.delete (addressFromState state) sheet else Map.insert (addressFromState state) (show editor) sheet}
  x ->
    state
      { editor = case x of
          KChar c -> editor {before = before ++ [c]}
          KBS | not (null before) -> editor {before = init before}
          KEnter -> editor {before = before ++ ['\n']}
          KRight | not (null after) -> editor {before = before ++ [head after], after = tail after}
          KLeft | not (null before) -> editor {after = last before : after, before = init before}
          _ -> editor
      }

handleMove :: Key -> State -> State
handleMove
  k
  state@State
    { selected = (selectedC, selectedR),
      scroll = (scrollC, scrollR),
      bounds = bounds@(cols, rows),
      sheet
    } = nstate {editor = Editor {before = fromMaybe "" $ Map.lookup (addressFromState nstate) sheet, after = ""}}
    where
      nstate =
        let (c, r) = (selectedC + mC, selectedR + mR)
            (gc, gr) = gridSize bounds
         in if r == -1
              then state {scroll = (scrollC, let new = scrollR - 1 in if new == (-1) then 0 else new)}
              else
                if r >= gr - 1
                  then state {scroll = (scrollC, scrollR + 1)}
                  else
                    if c == -1
                      then state {scroll = (let new = scrollC - 1 in if new == (-1) then 0 else new, scrollR)}
                      else
                        if c >= gc
                          then state {scroll = (scrollC + 1, scrollR)}
                          else state {selected = (c, r)}
      (mC, mR) = case k of
        KRight -> (1, 0)
        KLeft -> (-1, 0)
        KUp -> (0, -1)
        KDown -> (0, 1)
        _ -> (0, 0)

colWidth :: Int
colWidth = 10

editorHeight :: Int
editorHeight = 15

rowLabelsWidth :: Int
rowLabelsWidth = 5

gridSize :: (Int, Int) -> (Int, Int)
gridSize (cols, rows) = ((cols - rowLabelsWidth) `div` colWidth - 1, rows - editorHeight)

addressFromState
  state@State
    { scroll = (scrollCol, scrollRow),
      selected = (selectedCol, selectedRow)
    } = (scrollCol + selectedCol, scrollRow + selectedRow)

drawScreen :: State -> Picture
drawScreen
  state@State
    { bounds = bounds@(cols, rows),
      scroll = (scrollCol, scrollRow),
      selected = selected@(selectedCol, selectedRow),
      sheet,
      editor,
      editing
    } =
    emptyPicture
      { picLayers = [editorImage <|> (char labelAttr ' ' <-> charFill defAttr '│' 1 (editorHeight - 1)) <|> viewer, translateY editorHeight selectionImage, translateY editorHeight $ colLabels <-> (rowLabels <|> gridImage)],
        picBackground = Background ' ' defAttr
      }
    where
      editorImage = string labelAttr (take (cols `div` 2) ("editor:" ++ repeat ' ')) <-> drawEditor editor (cols `div` 2, editorHeight - 1)
      viewer = string labelAttr (take (cols `div` 2) ("viewer:" ++ repeat ' ')) <-> multiline defAttr selection
      bluebg = defAttr `withBackColor` brightBlue `withForeColor` white
      (gc, gr) = gridSize bounds
      address = addressFromState state
      selectionImage =
        translate
          (selectedCol * (colWidth + 1) + (rowLabelsWidth - 1))
          (selectedRow + 1)
          $ let mid =
                  trimmed
                    bluebg
                    colWidth
                    (if editing then "editing" else selection)
             in char bluebg ' ' <|> mid <|> char bluebg ' '
      selection = uncurry (cellToStr True) address
      gridImage =
        foldl1 divider $ map (\col -> colm textAttr colWidth $ map (cellToStr False col) [scrollRow .. (gr + scrollRow - 2)]) [scrollCol .. (scrollCol + gc)]
      cellToStr long col row = case Map.lookup (col, row) grid of
        Just (Right (Right litValue)) -> case litValue of
          String s -> s
          x -> show x
        Just (Right (Left evalError)) -> if long then show (sourceRange evalError) ++ ":\n" ++ "failed to evaluate\n" ++ show evalError else "EvalError"
        Just (Left parseError) -> if long then show parseError else "ParseError"
        Nothing -> ""

      grid :: Map.Map Address (Either ParseError (Either Expression LiteralValue))
      grid =
        snd $
          foldl
            ( \(env, literals) (address, source) ->
                (\x -> Map.insert address x literals) <$> case parse mainParser "" source of
                  Right exp -> case evaluate env exp of
                    Right (env2, litValue) -> (env2 {cells = Map.insert address litValue (cells env2)}, Right $ Right litValue)
                    Left x -> (env, Right $ Left x)
                  Left x -> (env, Left x)
            )
            (emptyEnviroment, mempty)
            (Map.toList sheet)

      labelAttr = defAttr `withStyle` standout
      textAttr = defAttr `withStyle` underline
      rowLabels = colm labelAttr rowLabelsWidth (map show $ take (gr - 1) [scrollRow ..])
      colLabels =
        string
          labelAttr
          ( replicate rowLabelsWidth ' '
              ++ concatMap
                (take (colWidth + 1) . (++ repeat ' ') . numToLetters)
                (take (gc + 1) [scrollCol ..])
          )
      a `divider` b = a <|> charFill textAttr '│' 1 gr <|> b

colm :: Attr -> Int -> [[Char]] -> Image
colm attr width = foldl1 (<->) . map (trimmed attr width)

trimmed :: Attr -> Int -> [Char] -> Image
trimmed attr width s = string attr $ take width $ s ++ repeat ' '

multiline attr [] = string attr ""
multiline attr s = foldl1 (<->) $ map (string attr) $ lines s

drawEditor :: Editor -> (Int, Int) -> Image
drawEditor Editor {before = before', after} (cols, rows) = vertCat $ map (horizCat . map (uncurry char)) all
  where
    before = before' ++ " "
    wraps = concatMap wrap
    wrap s =
      let maxCols = min (length s) cols
       in if maxCols == 0
            then []
            else take maxCols s : wrap (drop maxCols s)
    splitWhen _ [] = []
    splitWhen f s = front : splitWhen f (drop (length front + 1) s)
      where
        front = takeWhile f s
    all = wraps $ splitWhen (\(_, c) -> c /= '\n') $ map (defAttr,) (init before) ++ [(defAttr `withBackColor` brightBlue, last before)] ++ map (defAttr,) after
