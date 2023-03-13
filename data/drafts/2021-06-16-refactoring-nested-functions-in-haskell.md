---
title: Refactoring Nested Functions in Haskell
author: sanjiv sahayam
description:
tags: haskell
comments: true
---

While working on [Sharpen]() I came across the following code:

```
{-# LANGUAGE ScopedTypeVariables #-}

module ElmCompilerError (processError) where

import Prelude hiding (FilePath)
import Model
import RenderModel
import Theme
import ColorMap (maybeColor)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_)
import Control.Monad (when)

import qualified Data.List.NonEmpty as N

processError :: RuntimeConfig -> CompilerError -> IO ()
processError RuntimeConfig { runtimeConfigColorMap = colorMap, runtimeConfigConfig = config } compilerError =
  let stats            = configStats config
      numErrors        = configNumErrors config
      problems         = compilerErrorToProblemsAtFileLocations colorMap compilerError
      errorDescription = CompilerErrorDescription problems
  in renderCompilerErrorDescription stats numErrors errorDescription

-- unrelated functions here

renderCompilerErrorDescription :: Stats -> NumberOfErrors -> CompilerErrorDescription ->  IO ()
renderCompilerErrorDescription stats numErrors (CompilerErrorDescription errorDescriptions) = do
  traverse_ (\ed -> newLines 2 >> renderFileProblems ed) (filterByRequested numErrors errorDescriptions)
  newLines 2
  when (stats == StatsOn) $ do
    printNumberOfCompilationErrors (N.length errorDescriptions)
    newLines 2


filterByRequested :: NumberOfErrors -> N.NonEmpty ProblemsAtFileLocation -> [ProblemsAtFileLocation]
filterByRequested AllErrors = N.toList
filterByRequested OneError  = N.take 1
```


There was something off about this configuration of code and I didn't like it. I wasn't quite sure why I didn't like it though.

Here are the things I didn't like:
-
-

In `renderCompilerErrorDescription` we can extract two different functions

- We can remove the need to pass through the NumberOfErrors to this function because it's only used to figure out the errors to render
- We can move out the `when` block into another function so that we don't have to pass it in `Stats`

This will enable `renderCompilerErrorDescription` just render the compiler errors and not have to *filter out the inputs* and *calculate the stats*. It knew too much. It had to be broken apart.


```
renderCompilerErrorDescription :: CompilerErrorDescription ->  IO ()
renderCompilerErrorDescription (CompilerErrorDescription errorDescriptions) = do
  traverse_ (\ed -> newLines 2 >> renderFileProblems ed) errorDescriptions
  newLines 2
```

```
renderStats :: Stats -> Int -> IO ()
renderStats stats numberOfErrors =
  when (stats == StatsOn) $ do
    printNumberOfCompilationErrors numberOfErrors
    newLines 2
```

```
processError :: RuntimeConfig -> CompilerError -> IO ()
processError RuntimeConfig { runtimeConfigColorMap = colorMap, runtimeConfigConfig = config } compilerError =
  let stats                = configStats config
      numErrors            = configNumErrors config
      problems             = compilerErrorToProblemsAtFileLocations colorMap compilerError
      problemsToDisplay    = filterByRequested numErrors problems
      numProblemsDisplayed = N.length problemsToDisplay
      compilerErrorDesc    = CompilerErrorDescription problemsToDisplay

      renderEnabledStats   = renderStats stats numProblemsDisplayed
      renderCompilerErrors = renderCompilerErrorDescription compilerErrorDesc
  in renderCompilerErrors >> renderEnabledStats
```

- Each function should do only one thing
- Dependent functions should either be passed in or their results should be passed in
- Consider orchestration at highlevels as opposed to within lower level functions
