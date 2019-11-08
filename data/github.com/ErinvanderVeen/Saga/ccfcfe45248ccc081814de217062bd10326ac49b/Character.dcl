definition module Character

import StdBool
import StdInt
import StdTuple
import iTasks

:: Class = Barbarian | Bard | Cleric | Druid | Fighter | Monk | Paladin | Ranger | Rogue | Sorcerer | Warlock | Wizard
:: Level :== Int
:: Race = Dwarf | Elf | Halfling | Human | Dragonborn | Gnome | HalfElf | HalfOrc | Tiefling
:: Background = Acolyte | Charlatan | Criminal | Entertainer | FolkHero | Gladiator | GuildArtisan | Germit | Knight | Noble | Outlander | Pirate | Sage | Sailor | Soldier | Urchin
:: Morality = Good | Evil | NeutralM
:: Attitude = Lawful | Chaotic | NeutralA
:: Alignment :== (Morality, Attitude)

:: Characteristics =
	{ characterName :: String
	, classes :: [(Class, Level)]
	, race :: Race
	, background :: Background
	}

:: AbilityScores =
	{ strength :: (Int, Int)
	, dexterity :: (Int, Int)
	, constitution :: (Int, Int)
	, intelligence :: (Int, Int)
	, wisdom :: (Int, Int)
	, charisma :: (Int, Int)
	}

:: SavingThrows =
	{ strength :: Int
	, dexterity :: Int
	, constitution :: Int
	, intelligence :: Int
	, wisdom :: Int
	, charisma :: Int
	}

:: Skill :== (Bool, Int)
:: Skills =
	{ acrobatics :: Skill
	, animalHnadling :: Skill
	, arcana :: Skill
	, athletics :: Skill
	, deception :: Skill
	, history :: Skill
	, insight :: Skill
	, intimidation :: Skill
	, investigation :: Skill
	, medicine :: Skill
	, nature :: Skill
	, perception :: Skill
	, performane :: Skill
	, persuasion :: Skill
	, religion :: Skill
	, sleightOfHand :: Skill
	, stealth :: Skill
	, survival :: Skill
	}

:: Die = D4 | D6 | D8 | D10 | D12

:: DeathSaves =
	{ successes :: (Bool, Bool, Bool)
	, failures :: (Bool, Bool, Bool)
	}

:: CombatStats =
	{ armorClass :: Int
	, initiative :: Int
	, speed :: Int
	, maxHP :: Int
	, currentHP :: Int
	, tempHP :: Int
	, hitDice :: [(Die, Bool)]
	, deathSaves :: DeathSaves
	}

:: SkillStats =
	{ abilities :: AbilityScores
	, inspiration :: Bool
	, proficiencyBonus :: Int
	, savingThrows :: SavingThrows
	, skills :: Skills
	}

:: RoleplayCharacteristics =
	{ alignment :: Alignment
	, age :: Int
	, height :: Int
	, weight :: Int
	, eyes :: String
	, skin :: String
	, hair :: String
	}

:: RoleplayStats =
	{ roleplayCharacteristics :: RoleplayCharacteristics
	}

:: Character =
	{ characteristics :: Characteristics
	, skills :: SkillStats
	, combat :: CombatStats
	, roleplay :: RoleplayStats
	}

editCharacter :: Character -> Task Character
toTuple :: Character -> (Characteristics, SkillStats, CombatStats, RoleplayStats)
fromTuple :: (Characteristics, SkillStats, CombatStats, RoleplayStats) -> Character

derive class iTask Class, Race, Background, Morality, Attitude, Characteristics, AbilityScores, SavingThrows, Skills, Die, DeathSaves, CombatStats, SkillStats, RoleplayCharacteristics, RoleplayStats, Character
derive gDefault Class, Race, Background, Morality, Attitude, Characteristics, AbilityScores, SavingThrows, Skills, Die, DeathSaves, CombatStats, SkillStats, RoleplayCharacteristics, RoleplayStats, Character
