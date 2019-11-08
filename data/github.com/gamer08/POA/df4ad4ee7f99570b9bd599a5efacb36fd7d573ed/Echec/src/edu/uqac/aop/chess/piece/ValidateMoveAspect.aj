package edu.uqac.aop.chess.piece;
import edu.uqac.aop.chess.Board;
import edu.uqac.aop.chess.agent.HumanPlayer;
import edu.uqac.aop.chess.agent.Move;
import edu.uqac.aop.chess.agent.Player;

import java.time.chrono.IsoChronology;
import java.util.HashMap;
import java.util.Map;

public aspect ValidateMoveAspect 
{
	/*public pointcut ValidePawn(Piece p, Move m) : target(p) && execution(boolean Pawn.isMoveLegal(Move)) && args(m);
	
	before(Piece p, Move m) : ValidePawn(p, m)
	{
		System.out.println("Ceci est un test" + m.toString());
		p._validMove = true;
	}*/
	
	
	Map<Pawn, Boolean> _pawns = new HashMap<Pawn,Boolean>();
	Board _board;
	
	//Interception de la création d'un joueur pour pourvoir avoir une référence du plateau
	public pointcut HumanPlayerCreated(int color, Board b) : call(HumanPlayer.new(int, Board)) && args(color,b);
	
	before(int color, Board b) : HumanPlayerCreated(color, b)
	{
		this._board = b;
	}
	
	
	//Interception du constructeur des pions pour pouvoir gérer les premiers coups
	public pointcut PawnCreated() : call(Pawn.new(int));
	
	Pawn around() : PawnCreated()
	{
		Pawn p = (Pawn)proceed();
		_pawns.put(p, new Boolean(false));
		
		return p;
	}
	
	//Gestion du mouvement des pions
	//Le around sert pour prendre l'avantage sur la fonctionne surveillée. donc elle est utilisé à la place de celle surveillée
	
	//Le pion à 2 modes de déplacements un normal en ligne droite et la prise en diagonale, De plus, il ne peut pas reculer.
	
	boolean around(Pawn pawn, Move m) : target(pawn) && execution(boolean Pawn.isMoveLegal(Move)) && args(m)
	{
		boolean value = proceed(pawn, m);
		value = false;
		
		int xDisplacement = m.xF - m.xI;
		int yDisplacement = m.yF - m.yI;
		
		//tentative de prise par un pion
		if (Math.abs(xDisplacement) == 1)
		{
			if (Math.abs(yDisplacement) == 1)
			{
				switch (pawn.getPlayer()) 
				{
				case Player.BLACK:
					
					if ((yDisplacement) == -1)
					{
						if (_board.getGrid()[m.xF][m.yF].isOccupied())
						{
							if (_board.getGrid()[m.xF][m.yF].getPiece().getPlayer() == Player.WHITE)
								value = true;
						}
					}
					
					break;

				case Player.WHITE:
					if ((yDisplacement) == 1)
					{
						if (_board.getGrid()[m.xF][m.yF].isOccupied())
						{
							if (_board.getGrid()[m.xF][m.yF].getPiece().getPlayer() == Player.BLACK)
								value = true;
						}
					}
				}
			}
		}
		else if (xDisplacement == 0) // déplacement normal
		{
			if (Math.abs(yDisplacement) > 0 && Math.abs(yDisplacement) <= 2)
			{
				switch (pawn.getPlayer()) 
				{
				case Player.BLACK:
					
					if (yDisplacement < 0)
					{
						if (!_board.getGrid()[m.xF][m.yF].isOccupied())
						{
							if (yDisplacement == -2)  // test pour le déplacement initial
							{
								if (!_pawns.get(pawn).booleanValue())
								{
									_pawns.put(pawn,new Boolean(true));
									value = true;
								}
							}
							else
							{
								_pawns.put(pawn,new Boolean(true));
								value = true;
							}
						}
					}
					
					break;

				case Player.WHITE:
					
					if (yDisplacement > 0)
					{
						if (!_board.getGrid()[m.xF][m.yF].isOccupied())
						{
							if (yDisplacement == 2)  
							{
								if (!_pawns.get(pawn).booleanValue())
								{
									_pawns.put(pawn,new Boolean(true));
									value = true;
								}
							}
							else
							{
								_pawns.put(pawn,new Boolean(true));
								value = true;
							}
						}
					}
					
					break;
				}
			}
		}
		
		return value;
	}	
	
	//Gestion du mouvement des fous
	//Le around sert pour prendre l'avantage sur la fonctionne surveillée. donc elle est utilisé à la place de celle surveillée
	boolean around(Bishop bishop, Move m) : target(bishop) && execution(boolean Bishop.isMoveLegal(Move)) && args(m)
	{
		boolean value = proceed(bishop, m);
		value = true;
		
		int xDisplacement = m.xF - m.xI;
		int yDisplacement = m.yF - m.yI;
		
		int absoluteXDisplacement = Math.abs(xDisplacement);
		int absoluteYDisplacement = Math.abs(yDisplacement);
		
		if (absoluteXDisplacement == absoluteYDisplacement && (absoluteXDisplacement != 0 && absoluteYDisplacement !=0))
		{
			int xDirection = xDisplacement / absoluteXDisplacement;
			int yDirection = yDisplacement / absoluteYDisplacement;
			
			for (int i = 1 ; i <= absoluteXDisplacement ;i++)
			{
				if (_board.getGrid()[m.xI + (xDirection * i)][m.yI + (yDirection * i)].isOccupied())
				{
					if ((m.xI + (xDirection * i) == m.xF) && (m.yI + (yDirection * i) == m.yF))
					{
						if (_board.getGrid()[m.xF][m.yF].getPiece().getPlayer() == bishop.getPlayer())
							value = false;
					}
					else
					{
						value = false;
						break;
					}
				}
			}
		}
		else
			value = false;
		
		return value;
	}
	
	//Gestion du mouvement des chevaliers
	//Le around sert pour prendre l'avantage sur la fonctionne surveillée. donc elle est utilisé à la place de celle surveillée
	boolean around(Knight knight, Move m) : target(knight) && execution(boolean Knight.isMoveLegal(Move)) && args(m)
	{
		boolean value = proceed(knight, m);
		value = false;
		
		int xDisplacement = m.xF - m.xI;
		int yDisplacement = m.yF - m.yI;
		
		int absoluteXDisplacement = Math.abs(xDisplacement);
		int absoluteYDisplacement = Math.abs(yDisplacement);
		
		if ((absoluteXDisplacement == 1 && absoluteYDisplacement == 2) 
		|| (absoluteXDisplacement == 2 && absoluteYDisplacement == 1))
		{
			if ((!_board.getGrid()[m.xF][m.yF].isOccupied())
			|| (_board.getGrid()[m.xF][m.yF].isOccupied() && _board.getGrid()[m.xF][m.yF].getPiece().getPlayer() != knight.getPlayer()))
			{
				value = true;
			}
		}
		
		return value;
	}
	
	//Gestion du mouvement des tours
	//Le around sert pour prendre l'avantage sur la fonctionne surveillée. donc elle est utilisé à la place de celle surveillée
	boolean around(Rook rook, Move m) : target(rook) && execution(boolean Rook.isMoveLegal(Move)) && args(m)
	{
		boolean value = proceed(rook, m);
		value = true;
		
		int xDisplacement = m.xF - m.xI;
		int yDisplacement = m.yF - m.yI;
			
		int absoluteXDisplacement = Math.abs(xDisplacement);
		int absoluteYDisplacement = Math.abs(yDisplacement);
		
		if ((absoluteXDisplacement > 0  && absoluteYDisplacement == 0) 
		|| (absoluteXDisplacement == 0 && absoluteYDisplacement > 0))
		{
			int displacement, xDirection, yDirection;
			
			if (absoluteXDisplacement == 0)
			{
				displacement = absoluteYDisplacement;
				xDirection = 0;
				yDirection = yDisplacement / absoluteYDisplacement;
			}
			else
			{
				displacement = absoluteXDisplacement;
				xDirection = xDisplacement / absoluteXDisplacement;
				yDirection = 0;
			}
			
			for (int i = 1 ; i <= displacement ;i++)
			{
				if (_board.getGrid()[m.xI + (xDirection * i)][m.yI + (yDirection * i)].isOccupied())
				{
					if ((m.xI + (xDirection * i) == m.xF) && (m.yI + (yDirection * i) == m.yF))
					{
						if (_board.getGrid()[m.xF][m.yF].getPiece().getPlayer() == rook.getPlayer())
							value = false;
					}
					else
					{
						value = false;
						break;
					}
				}
			}
		}
		else
			value = false;
		
		return value;
	}
		
	//Gestion du mouvement des Rois
	//Le around sert pour prendre l'avantage sur la fonctionne surveillée. donc elle est utilisé à la place de celle surveillée
	boolean around(King king, Move m) : target(king) && execution(boolean King.isMoveLegal(Move)) && args(m)
	{
		boolean value = proceed(king, m);
		value = false;
		
		int xDisplacement = m.xF - m.xI;
		int yDisplacement = m.yF - m.yI;
			
		int absoluteXDisplacement = Math.abs(xDisplacement);
		int absoluteYDisplacement = Math.abs(yDisplacement);
		
		if ((absoluteXDisplacement == 1 && absoluteYDisplacement == 1) 
		|| (absoluteXDisplacement == 0 && absoluteYDisplacement == 1)
		|| (absoluteXDisplacement == 1 && absoluteYDisplacement == 0))
		{
			if ((!_board.getGrid()[m.xF][m.yF].isOccupied())
				|| (_board.getGrid()[m.xF][m.yF].isOccupied() && _board.getGrid()[m.xF][m.yF].getPiece().getPlayer() != king.getPlayer()))
			{
				value = true;
			}
		}
		
		return value;
	}
	
	//Gestion du mouvement des reines
	//Le around sert pour prendre l'avantage sur la fonctionne surveillée. donc elle est utilisé à la place de celle surveillée
	boolean around(Queen queen, Move m) : target(queen) && execution(boolean Queen.isMoveLegal(Move)) && args(m)
	{
		boolean value = proceed(queen, m);
		value = true;
		
		int xDisplacement = m.xF - m.xI;
		int yDisplacement = m.yF - m.yI;
			
		int absoluteXDisplacement = Math.abs(xDisplacement);
		int absoluteYDisplacement = Math.abs(yDisplacement);
		
		if ((absoluteXDisplacement > 0  && absoluteYDisplacement == 0) 
		|| (absoluteXDisplacement == 0 && absoluteYDisplacement > 0)
		|| ((absoluteXDisplacement == absoluteYDisplacement) && (absoluteXDisplacement != 0 && absoluteYDisplacement !=0)))
		{
			int displacement, xDirection, yDirection;
			
			if (absoluteXDisplacement == 0)
			{
				displacement = absoluteYDisplacement;
				xDirection = 0;
				yDirection = yDisplacement / absoluteYDisplacement;
			}
			else if (absoluteYDisplacement == 0)
			{
				displacement = absoluteXDisplacement;
				xDirection = xDisplacement / absoluteXDisplacement;
				yDirection = 0;
			}
			else
			{
				displacement = absoluteYDisplacement;
				xDirection = xDisplacement / absoluteXDisplacement;
				yDirection = yDisplacement / absoluteYDisplacement;
			}
			
			for (int i = 1 ; i <= displacement ;i++)
			{
				if (_board.getGrid()[m.xI + (xDirection * i)][m.yI + (yDirection * i)].isOccupied())
				{
					if ((m.xI + (xDirection * i) == m.xF) && (m.yI + (yDirection * i) == m.yF))
					{
						if (_board.getGrid()[m.xF][m.yF].getPiece().getPlayer() == queen.getPlayer())
							value = false;
					}
					else
					{
						value = false;
						break;
					}
				}
			}
		}
		else
			value = false;
		
		return value;
	}
}
