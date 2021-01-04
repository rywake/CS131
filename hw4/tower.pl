%%%%%%%%%% tower %%%%%%%%%%%%%%%%%%%%%%

%%% Get correct rows and cols %%%

len_row(X, N) :-
    length(X, N).

len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

within_domain([], _).
within_domain([HD | TL], N) :-
    fd_domain(HD, 1, N),
    within_domain(TL, N).

count_towers([],_,Count,Need) :-
    Count #= Need.
count_towers([Thd | Ttl],MAX,Count,Need) :-
    Thd #> MAX,
    Count1 is Count+1,
    count_towers(Ttl,Thd,Count1,Need).
count_towers([Thd | Ttl],MAX,Count,Need) :-
    Thd #< MAX,
    count_towers(Ttl,MAX,Count,Need).
check_counts([],[]).
check_counts([Thd | Ttl],[Chd | Ctl]) :-
    count_towers(Thd,0,0,Chd),
    check_counts(Ttl,Ctl).
check_counts_reverse([],[]).
check_counts_reverse([Thd | Ttl],[Chd | Ctl]) :-
    reverse(Thd,Rhd),
    count_towers(Rhd,0,0,Chd),
    check_counts_reverse(Ttl,Ctl).


%% Helper code from TA %%%
transpose([], []).
transpose([F|Fs], Ts) :-
transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


tower(N,T,C) :-
    len_row(T,N),
    len_col(T,N),
    within_domain(T,N),
    counts(Top,Bot,Left,Right) = C,
    length(Top,N),
    length(Bot,N),
    length(Left,N),
    length(Right,N),
    maplist(fd_all_different, T),
    transpose(T,TP),
    maplist(fd_all_different,TP),
    maplist(fd_labeling, T),
    check_counts(T,Left),
    check_counts_reverse(T,Right),
    check_counts(TP,Top),
    check_counts_reverse(TP,Bot).

%%%%%%%%%%%%%%%%%%%%% END TOWER %%%%%%%%%%%%%%%

%%%%%%%% PLAIN TOWER %%%%%%%%%%%% 

%%Helper code from TA hint %%%

within_domain_plain(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

%%Helper Code from TA %%%
unordered_range(N, Res) :-
    range_helper(N, Sorted),
    permutation(Sorted, Res).

fill_2d([], _,_,_).
fill_2d([Head | Tail], N,[C1hd | C1tl],[C2hd | C2tl]) :-
    within_domain_plain(N, Domain),
    permutation(Domain, Head),
    count_towers_plain(Head,0,0,C1hd),
    reverse(Head,Rhd),
    count_towers_plain(Rhd,0,0,C2hd),
    fill_2d(Tail, N,C1tl,C2tl).

count_towers_plain([],_,Count,Need) :-
    Count = Need.
count_towers_plain([Thd | Ttl],MAX,Count,Need) :-
    Thd > MAX,
    Count1 is Count+1,
    count_towers_plain(Ttl,Thd,Count1,Need).
count_towers_plain([Thd | Ttl],MAX,Count,Need) :-
    Thd < MAX,
    count_towers_plain(Ttl,MAX,Count,Need).

%%% helper code %%%
create_grid(Grid, N,C1,C2) :-
    length(Grid, N),
    fill_2d(Grid, N,C1,C2).

%%% Helper code from TA %%%
range_helper(0, []).
range_helper(N, [N | Rtl]) :-
    N > 0,  
    N1 is N - 1,
    range_helper(N1, Rtl). 

plain_tower(N,T,C) :-
    len_row(T,N),
    len_col(T,N),
    counts(Top,Bot,Left,Right) = C,
    length(Top,N),
    length(Bot,N),
    length(Left,N),
    length(Right,N),
    create_grid(T,N,Left,Right),
    transpose(T,TP),
    create_grid(TP,N,Top,Bot).

%%%%%%%%%%%%%%%%%%%%% END PLAIN_TOWER %%%%%%%%%%%%%%%%

%%%%% Speed-Up %%%%%%%%

speedup(Ratio) :-
    %%Time tower and store in T %%%
    statistics(cpu_time, [_, _]),
    tower(5,_,counts([2,3,2,1,2],
        [3,1,2,3,2],
        [4,1,2,2,2],
        [2,5,1,3,2])),
    statistics(cpu_time, [_ , T]),
    %%%Time plain_tower and store in PT %%%
    statistics(cpu_time, [_, _]),
    plain_tower(5,_,counts([2,3,2,1,2],
        [3,1,2,3,2],
        [4,1,2,2,2],
        [2,5,1,3,2])),
    statistics(cpu_time, [_ , PT]),
    Ratio is PT/T.

%%%%%%% END SPEED-UP %%%%%

%%% Ambiguous %%%

ambiguous(N,C,T1,T2) :-
tower(N,T1,C),
plain_tower(N,T2,C),
T1 \= T2.
%%%%%%%%%%%%%%%%%%%

