* Install Haskell Platform (latest version)
* Download QFeldspar from Github:

  > git clone https://github.com/shayan-najd/QFeldspar.git
* Update Cabal's local list of packages:

  > cabal update
* Install QFeldspar using cabal:

  > cd QFeldspar
  
  > cabal install
* [for Unix based systems only]

  For running files using GHCI, you possibly need to set the proper permissions for the downloaded files:
  
  >./setpermissions
