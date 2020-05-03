{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Library to get Haskell Language Extensions from @.cabal@ files, Haskell Modules,
or get combined information from both sources preserving correct merge logic of
extensions.
-}

module Extensions
    ( -- $types
      module Extensions.Types
      -- $package
    , module Extensions.Package
      -- $cabal
    , module Extensions.Cabal
      -- $module
    , module Extensions.Module
    ) where

import Extensions.Cabal
import Extensions.Module
import Extensions.Package
import Extensions.Types

{- $types
Main data types to work with any kind of extensions and all possible errors
during the work of @extensions@ functions.

To read more on Haskell Extensions, see the following documentation page:

 * [GHC Language Extensions](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html)

In @extensions@ we support both @Haskell2010@ and @SafeHaskell@
extensions.
-}

{- $package
Main functions to get information of used extensions by combined information
from @.cabal@ files and Haskell modules. This module includes functions that
could work with file paths or file sources, these functions kindly combine
extensions information in a convenient way. However, there are functions that
return Cabal and Module information separately for specified modules.

These functions use provided by "Extensions.Cabal" and "Extensions.Module"
functions to achieve that.
-}

{- $cabal
Functions to extract extensions from the @.cabal@ files.

Also, this module provides convenient functions for conversion of Cabal and GHC
extensions types.

-}

{- $module
Functions to extract extensions from the Haskell modules.

Also, this module exports convenient internal parsing functions for Haskell
module extensions and necessary statements.
-}
