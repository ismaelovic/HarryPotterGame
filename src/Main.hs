module Main where

import Data.List (intercalate, isPrefixOf, delete, find)
import Data.Char (toLower)
import Data.List (isPrefixOf, delete, find)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import Data.String (IsString)

--Colorize dependency
colorize :: Int -> String -> String
colorize code text = "\ESC[" ++ show code ++ "m" ++ text ++ "\ESC[0m"

red, green, yellow, blue, magenta, cyan :: String -> String
red = colorize 31
green = colorize 32
yellow = colorize 33
blue = colorize 34
magenta = colorize 35
cyan = colorize 36

-- Data types
data GameState = GameState {
    playerLocation :: Location,
    playerInventory :: [Item],
    worldItems :: [(String, [Item])],
    worldEnemies :: [(String, [Enemy])]
}

data Location = Location {
    locationName :: String,
    locationDescription :: String,
    locationExits :: [(String, String)]
}

data Enemy = Enemy {
    enemyName :: String,
    enemyHealth :: Int
}

type Item = String

initialState :: GameState
initialState = GameState {
    playerLocation = greatHall,
    playerInventory = [],
    worldItems = [
        ("Great Hall", ["Wand"]),
        ("Entrance Hall", ["Marauder's Map"]),
        ("Hogwarts Kitchens", ["Butterbeer", "Pumpkin Pasty"]),
        ("Gryffindor Common Room", ["Invisibility Cloak"]),
        ("Hogwarts Grounds", ["Broomstick"]),
        ("Forbidden Forest", ["Unicorn Hair"])
    ],
    worldEnemies = [
        -- ("Great Hall", [Enemy { enemyName = "Mischievous Mop", enemyHealth = 5 }]),
        -- ("Entrance Hall", [Enemy { enemyName = "Staircase Specter", enemyHealth = 10 }]),
        ("Hogwarts Kitchens", [Enemy { enemyName = "Culinary Poltergeist", enemyHealth = 7 }]),
        ("Gryffindor Common Room", [Enemy { enemyName = "Talkative Portrait", enemyHealth = 3 }]),
        ("Hogwarts Grounds", [Enemy { enemyName = "Whomping Willow", enemyHealth = 15 }]),
        ("Forbidden Forest", [Enemy { enemyName = "Lost Centaur", enemyHealth = 12 }])
    ]
}

-- Define locations
greatHall :: Location
greatHall = Location {
    locationName = "Great Hall",
    locationDescription = "You are in the Great Hall of Hogwarts. The enchanted ceiling shows a starry night sky.",
    locationExits = [("North", "Entrance Hall"), ("East", "Hogwarts Kitchens")]
}

entranceHall :: Location
entranceHall = Location {
    locationName = "Entrance Hall",
    locationDescription = "You're in the Entrance Hall. The grand staircase leads upwards, and you can see the door to the grounds.",
    locationExits = [("South", "Great Hall"), ("up", "Gryffindor Common Room"), ("out", "Hogwarts Grounds")]
}

kitchens :: Location
kitchens = Location {
    locationName = "Hogwarts Kitchens",
    locationDescription = "You've found the Hogwarts Kitchens. House-elves bustle about preparing meals.",
    locationExits = [("West", "Great Hall")]
}

gryffindorCommonRoom :: Location
gryffindorCommonRoom = Location {
    locationName = "Gryffindor Common Room",
    locationDescription = "You're in the cozy Gryffindor Common Room. A fire crackles in the fireplace.",
    locationExits = [("Down", "Entrance Hall")]
}

hogwartsGrounds :: Location
hogwartsGrounds = Location {
    locationName = "Hogwarts Grounds",
    locationDescription = "You're on the vast grounds of Hogwarts. You can see the Forbidden Forest in the distance.",
    locationExits = [("In", "Entrance Hall"), ("Forest", "Forbidden Forest")]
}

forbiddenForest :: Location
forbiddenForest = Location {
    locationName = "Forbidden Forest",
    locationDescription = "You've entered the dark and mysterious Forbidden Forest. Strange creatures lurk in the shadows.",
    locationExits = [("Back", "Hogwarts Grounds")]
}



-- Game logic
gameLoop :: GameState -> IO ()
gameLoop state = do
    action <- getPromptedAction state
    let (newState, message) = processAction action state
    unless (null message) $ putStrLn message
    if map toLower action == "quit" || action == "exit"
    then putStrLn $ red "Thanks for playing the Hogwarts Adventure!"
    else if not (locationName (playerLocation newState) == locationName (playerLocation state))
         then do
             printLocationInfo newState
             gameLoop newState
         else gameLoop newState

getPromptedAction :: GameState -> IO String
getPromptedAction state = do
    putStrLn $ magenta "\nWhat would you like to do?"
    getLine

printLocationInfo :: GameState -> IO ()
printLocationInfo state = do
    let currentLocation = playerLocation state
        exitDesc = intercalate ", " (map fst (locationExits currentLocation))
        itemsDesc = intercalate ", " (getLocationItems state (locationName currentLocation))
        enemiesDesc = intercalate ", " (map enemyName (getLocationEnemies state (locationName currentLocation)))
    putStrLn $ "\n" ++ yellow (locationName currentLocation)
    putStrLn $ yellow (locationDescription currentLocation)
    putStrLn $ cyan "Exits: " ++ blue exitDesc
    putStrLn $ cyan "Items: " ++ blue itemsDesc
    unless (null enemiesDesc) $ putStrLn $ red "Enemies: " ++ red enemiesDesc

processAction :: String -> GameState -> (GameState, String)
processAction action state
    | map toLower action == "quit" || action == "exit" = (state, "")
    | map toLower action == "inventory" = (state, yellow "You are carrying: " ++ green (show (playerInventory state)))
    | map toLower action == "look" = (state, lookAround state)
    | "go " `isPrefixOf` map toLower action = 
        let direction = drop 3 action
            exits = locationExits (playerLocation state)
        in case lookup (map toLower direction) (map (\(dir, loc) -> (map toLower dir, loc)) exits) of
            Just newLocName -> 
                let newLoc = fromMaybe (playerLocation state) (findLocation newLocName)
                in (state { playerLocation = newLoc }, green ("You go " ++ direction))
            Nothing -> (state, red "You can't go that way!" )
    | "take " `isPrefixOf` map toLower action =
        takeItem (drop 5 action) state
    | "kill " `isPrefixOf` map toLower action =
        killEnemy (drop 5 action) state
    | "drop " `isPrefixOf` map toLower action =
        dropItem (drop 5 action) state
    | map toLower action == "help" =
        (state, green ("This is an interactive text based game. Explore the Harry Potter themed locations and enemies." ++ 
                "\n\nYou can use the following commands:\n" ++
                "'go [direction]', 'take [item]', 'drop [item]', 'kill'[enemy], 'inventory', 'look', 'help', and 'quit'."))
    | otherwise = (state, red "I don't understand that command.\n ------------------")

lookAround :: GameState -> String
lookAround state =
    let currentLocation = playerLocation state
        exitDesc = yellow ("Exits: " ++ blue (intercalate ", " (map fst (locationExits currentLocation))))
        itemsDesc = yellow ("Items here: " ++ blue (intercalate ", " (getLocationItems state (locationName currentLocation))))
    in yellow (locationDescription currentLocation) ++ "\n" ++ exitDesc ++ "\n" ++ itemsDesc

takeItem :: String -> GameState -> (GameState, String)
takeItem item state =
    let currentLocationName = locationName (playerLocation state)
        currentItems = getLocationItems state currentLocationName
        lowerItem = map toLower item
        matchingItem = find (\i -> map toLower i == lowerItem) currentItems
    in case matchingItem of
        Just foundItem -> 
            (state { 
                playerInventory = foundItem : playerInventory state,
                worldItems = updateWorldItems state currentLocationName (delete foundItem currentItems)
             },
             green ("You take the " ++ foundItem ++ "."))
        Nothing -> (state, "Oops... There is no " ++ item ++ " here.")

dropItem :: String -> GameState -> (GameState, String)
dropItem item state =
    let currentLocationName = locationName (playerLocation state)
        currentInventory = playerInventory state
    in if item `elem` currentInventory
       then (state { 
                playerInventory = delete item currentInventory,
                worldItems = updateWorldItems state currentLocationName (item : getLocationItems state currentLocationName)
             },
             "You drop the " ++ item ++ ".")
       else (state, "You don't have a " ++ item ++ ".")

killEnemy :: String -> GameState -> (GameState, String)
killEnemy enemy state =
    let currentLocationName = locationName (playerLocation state)
        currentEnemies = getLocationEnemies state currentLocationName
        lowerEnemy = map toLower enemy
        enemyExists = any (\e -> map toLower (enemyName e) == lowerEnemy) currentEnemies
        newEnemies = filter (\e -> map toLower (enemyName e) /= lowerEnemy) currentEnemies
    in if enemyExists
        then (state { worldEnemies = updateWorldEnemies state currentLocationName newEnemies },
              green "You successfully killed " ++ red enemy)
        else (state, red ("There is no " ++ enemy ++ " here"))

-- Helper functions
findLocation :: String -> Maybe Location
findLocation name = case name of
    "Great Hall" -> Just greatHall
    "Entrance Hall" -> Just entranceHall
    "Hogwarts Kitchens" -> Just kitchens
    "Gryffindor Common Room" -> Just gryffindorCommonRoom
    "Hogwarts Grounds" -> Just hogwartsGrounds
    "Forbidden Forest" -> Just forbiddenForest
    _ -> Nothing

getLocationItems :: GameState -> String -> [Item]
getLocationItems state locName = fromMaybe [] (lookup locName (worldItems state))

updateWorldItems :: GameState -> String -> [Item] -> [(String, [Item])]
updateWorldItems state locName newItems = 
    (locName, newItems) : filter (\(name, _) -> name /= locName) (worldItems state)

getLocationEnemies :: GameState -> String -> [Enemy]
getLocationEnemies state locName = 
    fromMaybe [] (lookup locName (worldEnemies state))

updateWorldEnemies :: GameState -> String -> [Enemy] -> [(String, [Enemy])]
updateWorldEnemies state locName newEnemies = 
    (locName, newEnemies) : filter (\(name, _) -> name /= locName) (worldEnemies state)


-- Main function
main :: IO ()
main = do
    putStrLn "Welcome to the Hogwarts Adventure!"
    putStrLn "You can use commands like 'go [direction]', 'take [item]', 'drop [item]', 'inventory', 'look', 'help', and 'quit'."
    putStrLn "Type 'help' at any time to see this list again."
    printLocationInfo initialState
    gameLoop initialState