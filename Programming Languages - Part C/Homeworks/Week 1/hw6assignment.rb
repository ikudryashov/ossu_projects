# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.next_cheater_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end

  All_My_Pieces =
  [
    rotations([[0, 0], [-1, 0], [-1, -1], [0, -1], [1, 0]]),
    [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
     [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
    rotations([[0, 0], [0, -1], [1, 0]])
  ] + All_Pieces

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @cheated = false
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def cheat
    if @score >= 100 and !@cheated
      @score -= 100
      @cheated = true
    end
  end

  def next_piece
    if @cheated
      @current_block = MyPiece.next_cheater_piece(self)
      @cheated = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index|
      current = locations[index]
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
          @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180_degrees})
    @root.bind('c', proc {@board.cheat})
  end

end


