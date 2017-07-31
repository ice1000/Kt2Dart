
module Kt2Dart where

import Control.Applicative
import Control.Monad

import Parsers
import LexicalStructure
import {-# SOURCE #-} Modifiers
import {-# SOURCE #-} Types
import {-# SOURCE #-} Functions

kotlin2Dart = parseCode functionP
