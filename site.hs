{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

-- Main process
main :: IO ()
main = hakyll $ do

    -- Templates compilation
    match "templates/*" $ compile templateBodyCompiler

    -- Static images
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    -- Static pictograms
    match ("picto/*.png" .||. "picto/*.svg") $ do
      route   idRoute
      compile copyFileCompiler

    -- Css for styles and fonts
    match ("css/*.css" .||. "fonts/*/*.css") $ do
      route   idRoute
      compile compressCssCompiler

    -- Custom fonts
    match ("fonts/*/*.woff" .||. "fonts/*/*.woff2") $ do
      route   idRoute
      compile copyFileCompiler

    -- Seeded JSON files (from Planet)
    match "_seeds/api/*.json" $ do
      route (unseedRoute `composeRoutes` setExtension "json")
      compile copyFileCompiler

    -- Project seeded (from Planet)
    match projectsRule $ do
      route (unseedRoute `composeRoutes` setExtension "html")
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" projectContext
        >>= relativizeUrls
      
    -- Index.html
    match "index.html" $ do
      route idRoute
      compile $ do
        
        projects <- (recentFirst =<< loadAll projectsRule)
        
        let indexContext =
              listField "projects" projectContext (return projects) `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexContext
          >>= loadAndApplyTemplate "templates/default.html" indexContext
          >>= relativizeUrls



-- Remove artifacts in routes
unseedRoute = gsubRoute "_seeds/" (const "")

-- Custom Contexts

projectContext :: Context String
projectContext =
  bodyField      "body"              `mappend`
  titleField     "title"             `mappend`
  titleField     "name"              `mappend`
  teaserField    "teaser" "content"  `mappend`
  urlField       "url"               `mappend`
  pathField      "path"              `mappend`
  metadataField                      `mappend`
  missingField

-- Rules

projectsRule =
  "_seeds/projects/*.org"
  .||. "_seeds/projects/*.md"
  .||. "_seeds/projects/*.txt"
