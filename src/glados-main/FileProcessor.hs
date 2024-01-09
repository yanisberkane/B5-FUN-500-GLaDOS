module FileProcessor where

import VMBinaryFileProcess (writeStateToFile)
import VMTypes (VMEnv, Insts)

writeEnvAndInstsToFile :: FilePath -> (VMEnv, Insts) -> IO ()
writeEnvAndInstsToFile filename (env, insts) = writeStateToFile filename (env, insts)