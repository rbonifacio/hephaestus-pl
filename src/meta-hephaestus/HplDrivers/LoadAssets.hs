{-

This driver effectively tests whether the assets are separately compilable.
Assets must not have dependencies on any product.

-}

module Main where

import HplAssets.Hephaestus
import HplAssets.UseCases
-- The following modules still must be fixed for corrected dependencies.
-- import HplAssets.BusinessProcesses
-- import HplAssets.Components
-- import HplAssets.Requirements


main :: IO ()
main = do return ()
