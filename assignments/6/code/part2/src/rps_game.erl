-module(rps_game).
-export([coordLoop/7]).
% Finish
coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds, MoveWinner, LoseMsg) ->
    case gen_server:call(BrokerRef, are_we_draining) of
        yes -> 
            P1ID ! server_stopping, 
            P2ID ! server_stopping,
            gen_server:cast(BrokerRef, {done_draining, {P1ID, P2ID, self()}});
        no ->
            if 
                P1Score >= (MaxRounds/2) -> 
                            P1ID ! {game_over, P1Score, P2Score},
                            P2ID ! {game_over, P2Score, P1Score},
                            gen_server:cast(BrokerRef, {game_ended, PlayedRounds, {P1ID, P2ID, self()}});
                P2Score >= (MaxRounds/2) -> 
                            P1ID ! {game_over, P1Score, P2Score},
                            P2ID ! {game_over, P2Score, P1Score},
                            gen_server:cast(BrokerRef, {game_ended, PlayedRounds, {P1ID, P2ID, self()}});
                true ->
                    case MoveWinner of
                        none -> 
                            P1ID ! tie,
                            P2ID ! tie,
                            coordLoop(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds);
                        p1 ->
                            P1ID ! win,
                            P2ID ! {loss,LoseMsg},
                            coordLoop(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds);
                        p2 ->
                            P1ID ! {loss,LoseMsg},
                            P2ID ! win,
                            coordLoop(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds)
                    end
                end
    end.
% 判断本剧游戏输赢，并调用EvalRound
% Finish
coordEvalMoves(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds, P1Move, P2Move) ->
    Validmoves = [rock, scissors, paper],
    ValidP1 = lists:member(P1Move, Validmoves),
    ValidP2 = lists:member(P2Move, Validmoves),
    if  
        ValidP1 == false ->
            coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score+1, MaxRounds, PlayedRounds, p2, invalid_input);
        ValidP2 == false ->
            coordEvalRound(BrokerRef, P1ID, P2ID, P1Score+1, P2Score, MaxRounds, PlayedRounds, p1, invalid_input);
        true ->
            case {P1Move, P2Move} of
                {Same, Same} -> 
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds, none, tie);
                {rock, scissors} -> 
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score+1, P2Score, MaxRounds, PlayedRounds, p1, rock);
                {scissors, paper} -> 
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score+1, P2Score, MaxRounds, PlayedRounds, p1, scissors);
                {paper, rock} -> 
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score+1, P2Score, MaxRounds, PlayedRounds, p1, paper);
                {scissors, rock} -> 
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score+1, MaxRounds, PlayedRounds, p2, rock);
                {paper, scissors} -> 
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score+1, MaxRounds, PlayedRounds, p2, scissors);
                {rock, paper} -> 
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score+1, MaxRounds, PlayedRounds, p2, paper)
            end
    end.

% Finish
coordLoop(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds) ->
    receive
        {P1ID, {move, P1Move}} -> P1M = P1Move
    end,
    receive
        {P2ID, {move, P2Move}} -> P2M = P2Move
    end,
    coordEvalMoves(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds+1, P1M, P2M).
