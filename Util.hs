module Util where

isSingleton xs = length xs == 1

deleteEmptySublists = filter (not . null)
