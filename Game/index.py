from game_character import GameCharacter
from game import Game
from random import randint


def main():
    character1 = GameCharacter("Mario", "burned", 100, None, 0)
    character2 = GameCharacter("Luigi", "frozen", 100, None, 0)
    # character1.displayCharacter()

    # Loop with attacking
    while(character1.hp > 0 and character2.hp > 0):
        character1_move = input(
            "[{}] Input A for Attack, S for Special Attack, or R for Restore\n".format(character1.name))
        character2_move = input(
            "[{}] Input A for Attack, S for Special Attack, or R for Restore\n".format(character2.name))
        # Determines who gets to do their move first for each round
        striker = randint(1, 2)
        # If character 1 is picked to go first
        if striker == 1:
            # character 1
            Game.gameMove(character1, character2, character1_move)
            # Checks if they have a special status to damage them
            if character2.status:
                Game.status_damage(character2)

            if character2.hp <= 0:
                print("{} has died, {} wins!".format(
                    character2.name, character1.name))
                break
             # character 2
            Game.gameMove(character2, character1, character2_move)
            # Checks if they have a special status to damage them
            if character1.status:
                Game.status_damage(character1)
            if character1.hp <= 0:
                print("{} has died, {} wins!".format(
                    character1.name, character2.name))
                break

        # If characer 2 goes first
        if striker == 2:
            # character 2
            Game.gameMove(character2, character1, character2_move)
                # Checks if they have a special status to damage them
            if character1.status:
                Game.status_damage(character1)
            if character1.hp <= 0:
                print("{} has died, {} wins!".format(
                    character1.name, character2.name))
                break

            # character 1
            Game.gameMove(character1, character2, character1_move)
                # Checks if they have a special status to damage them
            if character2.status:
                Game.status_damage(character2)
            if character2.hp <= 0:
                print("{} has died, {} wins!".format(
                    character1.name, character2.name))
                break

        print("[{}] Health: {}".format(character1.name, character1.hp))
        print("[{}] Health: {}".format(character2.name, character2.hp))
    # character1.displayCharacter()
    # character2.displayCharacter()


if __name__ == "__main__":
    main()
