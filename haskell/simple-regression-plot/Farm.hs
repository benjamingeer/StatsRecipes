module Main where

{-

Reads x and y values from a two-column CSV file, does a least-squares
linear regression and generates a gnuplot script that will plot the
data as a scatterplot along with the regression line. Uses the Haskell
wrapper for the GNU Scientific Library to do the linear
regression. Generates the gnuplot script from a template using
HStringTemplate. The generated gnuplot script uses gnuplot's epslatex
terminal to produce LaTeX with Embedded PostScript, which can then be
included in a LaTeX document. (I tried the Haskell gnuplot wrapper,
but it doesn't support the epslatex terminal, and in any case this
approach is actually much simpler and clearer.)

-}

import Data.List
import Data.Maybe (maybe)
import System.Environment (getArgs)
import System.Console.GetOpt
import Text.CSV.ByteString (CSV, parseCSV)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lex.Double as BD
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Packed.Vector as Vector
import qualified Numeric.GSL.Fitting.Linear as Linear
import qualified Text.StringTemplate as Templ

main = do
  -- Get the command line args
  args <- getArgs
  let opts = getPlotOpts args
  let dataFile = getDataFile opts
  let templateFile = getTemplateFile opts
  let scriptFile = getScriptFile opts
  let outputFile = getOutputFile opts
  
  -- Read the data file
  csvStr <- B.readFile dataFile
  let (xs, ys) = case parseCSV csvStr of
        Nothing -> error $ "Couldn't parse CSV file" ++ dataFile
        Just csv -> csvToDoubles csv
        
  -- Do the calculation
  let (intercept, slope) = linearRegression xs ys
  
  -- Generate a gnuplot script
  let strAttrs = [ ("outputFile", outputFile),
                   ("intercept", show intercept),
                   ("slope", show slope), 
                   ("dataFile", dataFile) ]
  templateStr <- readFile templateFile
  let template = Templ.setManyAttrib strAttrs (Templ.newSTMP templateStr)
  writeFile scriptFile (Templ.render template)
  
  
-- Command-line option processing
  
data Flag = DataFile String |
            TemplateFile String |
            ScriptFile String |
            OutputFile String
          deriving Show

options :: [OptDescr Flag]
options =
  [ Option "d"  ["data"]   (ReqArg DataFile "FILE") "data FILE",
    Option "t"  ["template"] (ReqArg TemplateFile "FILE") "template FILE",
    Option "s"  ["script"] (ReqArg ScriptFile "FILE") "script FILE",
    Option "o"  ["output"] (ReqArg OutputFile "FILE") "output FILE" ]

getPlotOpts :: [String] -> [Flag]
getPlotOpts args =
  case getOpt Permute options args of
    (o, [], []) -> o
    (_, _, errs) -> usageError (concat errs)

usageError s =
  error (s ++ usageInfo header options)
    where header = "Usage: Farm [OPTION...]"

getDataFile :: [Flag] -> String
getDataFile opts =
  maybe (usageError "data filename required\n")
  (\(DataFile val) -> val) (find finder opts)
    where finder (DataFile _) = True
          finder _ = False

getTemplateFile :: [Flag] -> String
getTemplateFile opts =
  maybe (usageError "template filename required\n")
  (\(TemplateFile val) -> val) (find finder opts)
    where finder (TemplateFile _) = True
          finder _ = False

getScriptFile :: [Flag] -> String
getScriptFile opts =
  maybe (usageError "script filename required\n")
  (\(ScriptFile val) -> val) (find finder opts)
    where finder (ScriptFile _) = True
          finder _ = False

getOutputFile :: [Flag] -> String
getOutputFile opts =
  maybe (usageError "output filename required\n")
  (\(OutputFile val) -> val) (find finder opts)
    where finder (OutputFile _) = True
          finder _ = False

-- Parse the numbers in the data file
csvToDoubles :: CSV -> ([Double], [Double])
csvToDoubles csv =
  foldr convRow ([], []) csv where
    convRow row (xAcc, yAcc) = case row of
      x:y:[] -> (fieldToDouble x : xAcc,
                 fieldToDouble y : yAcc)
      _ -> error "Expected two colums per row"
      where fieldToDouble field =
              case BD.readDouble field of
                Nothing -> error $ "Couldn't parse field" ++
                           Text.unpack (decodeUtf8 field)
                Just (value, _) -> value

-- Fit the regression line
linearRegression :: [Double] -> [Double] -> (Double, Double)
linearRegression xs ys =
  let (intercept, slope, cov00, cov01, cov11, chiSq) =
        Linear.linear (Vector.fromList xs) (Vector.fromList ys)
  in (intercept, slope)
