{-# LANGUAGE DeriveDataTypeable #-}

module Main where

{-

Reads x and y values from a two-column CSV file, does a least-squares
linear regression and generates a gnuplot script that will plot the
data as a scatterplot along with the regression line. Uses the Haskell
wrapper for the GNU Scientific Library to do the linear
regression. Generates the gnuplot script from a template using
HStringTmplate. The generated gnuplot script uses gnuplot's epslatex
terminal to produce LaTeX with Embedded PostScript, which can then be
included in a LaTeX document. (I tried the Haskell gnuplot wrapper,
but it doesn't support the epslatex terminal, and in any case this
approach is actually much simpler and clearer.)

-}

import System.Console.CmdArgs.Implicit
import Control.Monad (unless)
import Text.CSV.ByteString (CSV, parseCSV)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lex.Double as BD
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Packed.Vector as Vector
import qualified Numeric.GSL.Fitting.Linear as Linear
import qualified Text.StringTemplate as Tpl
import System.FilePath (splitExtension)

main = do
  -- Get the command line args
  args <- cmdArgs farm
  let errs = checkArgs args in
    unless (null errs) $ error (unlines errs)

  -- Read the data file
  csvStr <- B.readFile (dataFile args)
  let (xs, ys) = case parseCSV csvStr of
        Nothing -> error $ "Couldn't parse CSV file" ++ dataFile args
        Just csv -> csvToDoubles csv
        
  -- Do the calculation
  let (intercept, slope) = linearRegression xs ys
  
  -- Generate a gnuplot script
  let attrs = [ ("outputFile", outputFile args),
                ("intercept", show intercept),
                ("slope", show slope), 
                ("dataFile", dataFile args) ]
  templates <- Tpl.directoryGroup "./" :: IO (Tpl.STGroup B.ByteString)
  let Just template = Tpl.getStringTemplate
                      (fst (splitExtension (templateFile args)))
                      templates
  B.writeFile (scriptFile args)
    (Tpl.render (Tpl.setManyAttrib attrs template))

  
-- Command-line option processing

data Farm = Farm { dataFile :: String,
                   templateFile :: String,
                   scriptFile :: String,
                   outputFile :: String }
          deriving (Data, Typeable, Show, Eq)
  
farm = Farm {
  dataFile = def &= explicit &= name "d" &= name "data" &= typFile
             &= help "data file in CSV format",
  templateFile = def &= explicit &= name "t" &= name "template" &= typFile
                 &= help "template file",
  scriptFile = def &= explicit &= name "s" &= name "script" &= typFile
               &= help "gnuplot script file to generate",
  outputFile = def &= explicit &= name "o" &= name "output" &= typFile
               &= help "LaTeX output file to generate" }

checkArgs :: Farm -> [String]
checkArgs args =
  ["data filename required" | null (dataFile args)] ++
  ["template filename required" | null (templateFile args)] ++
  ["script filename required" | null (scriptFile args)] ++
  ["output filename required" | null (outputFile args)]

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
