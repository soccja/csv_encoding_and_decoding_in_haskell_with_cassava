# csv_encoding_and_decoding_in_haskell_with_cassava

Cassava library demo based on <https://www.stackbuilders.com/insights/csv-encoding-decoding/>

## Preamble

I found [Encoding and decoding in Haskell with Cassava](https://www.stackbuilders.com/insights/csv-encoding-decoding/) to be an excellent resource but it needs some TLC to get the most from it.

Issues include that the referenced [link](https://www.data.gov/app/uploads/2016/02/opendatasites.cs) to the `opendatasites.csv` dataset used as test data is broken.

## Configure using git clone

### Clone the repository

```
pushd ~                                                                                             # Go to home directory
git clone https://github.com/soccja/csv_encoding_and_decoding_in_haskell_with_cassava/tree/main     # Create a local copy of the repository
```

### Configure cabal

#### Determine installed and active base version

```
ghcup list 2>/dev/null|grep '✔✔'|grep 'base'   # Show what version of ghc and associated base are installed and active
```

> ✔✔ ghc 9.12.2 base-4.21.0.0 hls-powered,2025-03-12

#### Edit cabal configuration file

- Change references to `build-depends:   base ^>=4.21.0.0` in `csvdemo.cabal` to match the version of ghc you are using.

## Configure 'manually'

### Obtain a copy of the test data

- Source: <https://github.com/geominr/open-data/blob/master/opendatasites.csv>
- Download `opendatasites.csv` and then apply `csvcut` to extract the data used by the haskell module:

  ```
  csvcut -c name,Link,Type opendatasites.csv > /tmp/items.csv
  ```

- Edit `/tmp/items.csv`. Rename `name` to `Item` in the first row of the file (that is, the header).

### Configure the environment

```
cabal update            # Refresh package list for known remote repositories
```

> Downloading the latest package list from hackage.haskell.org  
> Package list of hackage.haskell.org has been updated.  
> The index-state is set to 2026-01-28T03:53:25Z.  
> To revert to previous state run:  
>   cabal v2-update 'hackage.haskell.org,2026-01-28T01:35:38Z'  
> cabal update 5.08s user 0.78s system 89% cpu 6.524 total

### Create a new cabal package

```
cabal init
```

> [!NOTE]
>
> - To "What does the package build?" I selected 3) Library and Executable. I chose defaults for all other questions.
> - I renamed `MyLib.hs` to `OpenData.hs` and replaced references to `MyLib` with `OpenData` in `csvdemo.caval`, `Main.hs` and `OpenData.hs`

### Edit `OpenData.hs` and `Main.hs`

Edit contents of `OpenData.hs` and `Main.hs` to match what is in this repository.

### Add library dependencies to cabal configuration

The following dependencies need to be added to `csvdemo.cabal`:

- bytestring
- cassava
- text
- vector

## Build and run

```
cabal build
cabal run
```

## Other references

- <http://etorreborre.blogspot.com/2019/09/processing-csv-files-in-haskell.html>
