# csv_encoding_and_decoding_in_haskell_with_cassava

Cassava library demo based on https://www.stackbuilders.com/insights/csv-encoding-decoding/

## Preamble

I found [Encoding and decoding in Haskell with Cassava](https://www.stackbuilders.com/insights/csv-encoding-decoding/) to be an excellent resource but it needs some TLC to get the most from it.

Issues include that the [link](https://www.data.gov/app/uploads/2016/02/opendatasites.cs)  to the `opendatasites.csv` dataset referenced in the article is broken.

## Obtain a copy of `opendatasites.csv`

- Source: https://github.com/geominr/open-data/blob/master/opendatasites.csv
- Download `opendatasites.csv` and then apply `csvcut` to extract the data used by the haskell module:
  ```
  csvcut -c name,Link,Type opendatasites.csv > /tmp/items.csv
  ```
- Edit `/tmp/items.csv`. Rename `name` to `Item`
## Configure environment
```
mkdir ~/csvdemo
pushd ~/csvdemo
mv /tmp/items.csv .     # Get the open-data
ghcup list|grep '✔✔'
```

> ✔✔ ghc   9.12.2      base-4.21.0.0             hls-powered,2025-03-12  
> ✔✔ cabal 3.16.1.0    latest  
> ✔✔ hls   2.12.0.0  
> ✔✔ stack 3.3.1  
> ✔✔ ghcup 0.1.50.2    latest,recommended

```
cabal update
```
> Downloading the latest package list from hackage.haskell.org  
> Package list of hackage.haskell.org has been updated.  
> The index-state is set to 2026-01-28T03:53:25Z.  
> To revert to previous state run:  
>   cabal v2-update 'hackage.haskell.org,2026-01-28T01:35:38Z'  
> cabal update  5.08s user 0.78s system 89% cpu 6.524 total

```
cabal init
```
> [!NOTE]
> * To "What does the package build?" I selected 3) Library and Executable. I chose defaults for all other questions.
> * I renamed `MyLib.hs` to `OpenData.hs` and replaced references to `MyLib` with `OpenData` in `csvdemo.caval`, `Main.hs` and `OpenData.hs`

> [!WARNING]
> If you are cloning this repository, change references to `build-depends:   base ^>=4.21.0.0` in `csvdemo.cabal` to match the version of ghc you are using.  
> (See above output from running `ghcup list`.)

## Add library dependencies to `csvdemo.cabal`
The following dependencies need to be added:
- bytestring
- cassava
- text
- vector


## Edit `OpenData.hs` and `Main.hs`

Edit contents of `OpenData.hs` and `Main.hs` to match what is in this repository.

## Run the demo
```
cabal build
cabal run
```

## Other references
- http://etorreborre.blogspot.com/2019/09/processing-csv-files-in-haskell.html

