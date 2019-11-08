use("ispec")
use("lib/machine_player")

describe("MachinePlayer",

    before(
      player = MachinePlayer mimic(1)
      board = Board mimic(3)
      )

    it("can be initialized with a marker value",
      player marker_value should == 1
      )

    it("calculates the inverse of a number",
      MachinePlayer inverse(4) should == 1/4
      MachinePlayer inverse(-4) should == -1/4
      MachinePlayer inverse(0) should == 0
      )

    it("gets the first value of a dict",
      move = {[2,1] => 1/5}
      MachinePlayer get_score(move) should == 1/5
      )

    it("knows if the other player took middle",
      player other_player_took_middle(board) should be false

      board set_space(1,1,-1)

      player other_player_took_middle(board) should be true
      )

    it("knows of an immediate winning move",
      board set_space(0,0,1)
      board set_space(0,1,1)

      player immediate_winning_move(board) should == [0,2]
      )

    it("knows of an immediate blocking move",
      board set_space(0,0,-1)
      board set_space(0,1,-1)

      player immediate_blocking_move(board) should == [0,2]
      )

    it("compares scores based on the player's marker value",
      MachinePlayer better_move_than_best_move?(1/3, {[2,1] => -1/8}, 1) should be true
      MachinePlayer better_move_than_best_move?(1/3, {[2,1] => -1/8}, -1) should be false
      )

    it("calculates the score of a move based on how many moves it takes to get the game to end",
      board set_space(0,0,1)
      board set_space(0,1,-1)
      board set_space(0,2,-1)
      board set_space(1,0,1)
      board set_space(1,1,1)
      board set_space(1,2,-1)
      board set_space(2,0,1)
      board set_space(2,1,1)
      board set_space(2,2,0)
      depth = 3
      MachinePlayer move_score(board, depth) should == 1/3
      )

    it("will pick a corner if the board is blank",
      board corners include?(player get_move(board)) should be true
      )

    it("takes the middle on first move if it goes 2nd",
      player2 = MachinePlayer mimic(-1)

      board set_space(0,0,1)

      player2 get_move(board) should == [1,1]
      )

    it("will win if it can",
      board set_space(0,0,1)
      board set_space(0,1,-1)
      board set_space(1,1,1)
      board set_space(0,2,-1)
      player get_move(board) should == [2,2]
      )

    it("blocks when it should",
      board set_space(0,0,-1)
      board set_space(1,1,1)
      board set_space(1,0,-1)
      player get_move(board) should == [2,0]
      )

    it("wins an easy one",
      board set_space(0,0,-1)
      board set_space(0,1,1)
      board set_space(0,2,-1)
      board set_space(1,0,-1)
      board set_space(1,1,1)
      board set_space(1,2,-1)
      board set_space(2,0,1)
      player get_move(board) should == [2,1]
      )

    it("blocks an easy one",
      board set_space(0,0,1)
      board set_space(0,1,-1)
      board set_space(0,2,1)
      board set_space(1,0,-1)
      board set_space(1,1,-1)
      board set_space(2,0,-1)
      board set_space(2,1,1)
      player get_move(board) should == [1,2]
      )
    )
