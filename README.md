# Pokémon Emerald: Nuzlocke Mod

This is a mod for Pokemon Emerald built using pret's pokeemerald decompilation. It turns the game into a "Nuzlocke" challenge, with the rules enforced by the game rather than the player.

## Features:
* Only the first Pokemon you encounter in a route is catchable. If you attempt to catch a Pokemon on a subsequent encounter, the game will stop from from throwing a Ball.
* When a Pokemon faints, it is gone forever. During the battle in which it fainted, it cannot be revived. At the end of the battle, all fainted Pokemon are permanently deleted. If you run out of all of your Pokemon and white out, the game will search through your Boxes and move the first Pokemon it finds into your party. If you have no Pokemon in your Box when you white out, the game will soft reset.
* A "species clause" has been implemented; if your first encounter is a Pokemon in the same evolutionary line as a Pokemon you have already caught, you will not be allowed to capture it, and your encounter for the area will not be used.
* A "shiny clause" is also in effect which overrides everything. If a Pokemon is shiny, you are allowed to capture it no matter what.
* The Nuzlocke run is controlled by a global switch that turns ON when you first receive the Pokedex and turns OFF when you beat Steven in Meteor Falls. This way, your first encounter on Route 101 is not wasted before you can capture it, and after you have beaten Steven, you can play the game as you normally would, without the restrictions of a Nuzlocke.
* Not directly related to the Nuzlocke, but the RNG is now properly seeded from the RTC, so you won't get RNG clones anymore.

## Building
Building is identical to building regular pokeemerald. If you are not familiar with the build process, look at the [pret repository] (https://GitHub.com/pret/pokeemerald).
