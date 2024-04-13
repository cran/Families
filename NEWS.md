# Families 1.0.2 Published July 8 2022

# Families 2.0.0 Submitted March 2024: changes in version 2.0.0

## New functions

-   Age()
-   Alive()
-   IDau()
-   IDgch()
-   IDsib()
-   Kin\_long()
-   Nkin()
-   PlotAges()
-   Tests()

## Functions removed:

-   eo(): the function is now part of VirtualPop
-   Multistate()

## Data

-   Removed:
    -   rates, which is available in VirtualPop
    -   dpopus, which was used in the vignette
-   Changed: dataLH\_F is replaced by dLH (USA 2021)

## Vignette

The vignette “Families and Kinship Networks in Virtual Populations. The
‘Families’ package.” has been extended and is now a separate paper,
which will hopefully be published in the coming months.

# Families 2.0.1 Submitted March 2024: Changes in version 2.0.1

## Functions updates:

-   IDhc() Improved readability
-   IDghc() Completely rewritten for improved readability and correction
    of a bug
-   Tests() A new test added: an idego value outside of the acceptable
    range results in a warning an the omission of the unacceptable
    values.

# Families 2.0.2 April 2024

## Function updates

Tests(): Correct warning message. Some IDs may be missing, e.g. ego
without cousins has no IDs of cousins. Originally, ego was removed from
the list of ego-cousin dyads. That caused vectors of unequal length and
warning “longer object length is not a multiple of shorter object
length”. That issue is resolved by including the ego-NA dyads. The
change prevents unnecessary warnings in Db() and Alive()
