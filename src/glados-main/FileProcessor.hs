module FileProcessor (
    -- *FileProcessor
    -- $fileprocessor
    writeEnvAndInstsToFile
)where

import VMBinaryFileProcess (writeStateToFile)
import VMTypes (VMEnv, Insts)

{- $fileprocessor
    This module contains the file processor used in the Glados interpreter.
-}

writeEnvAndInstsToFile :: FilePath -> (VMEnv, Insts) -> IO () -- ^ Write the environment and the instructions into a file.
writeEnvAndInstsToFile filename (env, insts) = writeStateToFile filename (env, insts)