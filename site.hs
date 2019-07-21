--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
unseedRoute = gsubRoute "_seeds/" (const "")

main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateBodyCompiler
    
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match ("picto/*.png" .||. "picto/*.svg") $ do
        route   idRoute
        compile copyFileCompiler

    match ("css/*.css" .||. "fonts/*/*.css") $ do
        route   idRoute
        compile compressCssCompiler

    match ("fonts/*/*.woff" .||. "fonts/*/*.woff2") $ do
        route   idRoute
        compile copyFileCompiler
 
    match "_seeds/api/*.json" $ do
        route (unseedRoute `composeRoutes` setExtension "json")
        compile copyFileCompiler

    match "index.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
