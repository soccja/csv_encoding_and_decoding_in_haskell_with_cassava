{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module Main (main) where

module Main (
    main,
)
where

-- We don't need a lot of imports:

-- open-data
import OpenData

-- base
import qualified Control.Monad as Monad
import qualified System.Exit as Exit

-- And we can simply decode the items.csv file and then print the length of the result of filtering the country items:

main :: IO ()
main = do
    putStrLn "Open data!"

    eitherCountryItems <-
        fmap filterCountryItems
            <$> decodeItemsFromFile "items.csv"

    case eitherCountryItems of
        Left reason ->
            Exit.die reason
        Right countryItems -> do
            putStr "Number of country items: "
            print (length countryItems)

            Monad.void (encodeItemsToFile "countries.csv" countryItems)
