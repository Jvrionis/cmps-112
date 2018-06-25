%====================================================================
%  James Vrionis 
%  W.Mackey
%  CMPS112-WM
%  ASG4
%   
%  functions.pl: An airline reservation system for the 
%  Twilight Zone Airlines.
%--------------------------------------------------------------------

%====================================================================
% Radian Conversion
%--------------------------------------------------------------------
radian( degmin( Deg, Min ), Conversion ) :-
    Conversion is (Deg + Min / 60) * pi / 180.

%--------------------------------------------------------------------

%====================================================================
% Handling NOT, 
% Cut which acts like true, but cannot be backtracked past. 
%--------------------------------------------------------------------
not( X ) :- X, !, fail.
not( _ ).
%--------------------------------------------------------------------

%====================================================================
% Computes the distance between airports using haversine formula
% Lat(*): Lattitude, Lng(*): Longitude, Dlat: Delta lattitude,
% Dlng: Delta Longitude, 
%
%--------------------------------------------------------------------
haversine( Lat1, Lng1, Lat2, Lng2, Distance ) :-
    radian( Lat1, Radlat1 ), radian( Lng1, RadLng1 ),
    radian( Lat2, RadLat2 ), radian( Lng2, RadLng2 ),
    Dlat is RadLat2 - Radlat1, Dlng is RadLng2 - RadLng1,
    A is sin( Dlat / 2 ) ** 2
        + cos( Radlat1 ) * cos( RadLat2 ) * sin( Dlng / 2 ) ** 2,
    Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
    Distance is Dist * 3961.
%--------------------------------------------------------------------

%====================================================================
% Different Time conversions and Print_time
% 30 minute transfer time between other flights.
% NDH: Next Departure {Hours, Minutes}
% Flights must not be longer than 24 hours according to 
% Twilight Time Zone
%--------------------------------------------------------------------
min_boundry( Time ) :-
    Time < 10, 
    print( 0 ), print( Time ).

min_boundry( Time ) :-
    Time >= 10, print( Time ).

hours( Hours, Minutes, HM ) :-
    HM is Hours + Minutes / 60.

minutes( Hours, Minutes ) :-
    Minutes is Hours * 60.

offset( Hours, Minutes, ArrHours, ArrMinutes ) :-
    Minutes >= 60, ArrHours is Hours + 1,
    ArrMinutes is Minutes - 60.

offset( Hours, Minutes, ArrHours, ArrMinutes ) :-
    Minutes < 60, ArrHours is Hours,
    ArrMinutes is Minutes.

thiry_min( DurHrs, time( NDH, NDM )) :-
    hours( NDH, NDM, Hrs ),
    minutes( DurHrs, DurMins ),
    minutes( Hrs, Mins ),
    DurMins + 29 < Mins.

max_airtime( flight( POP, POT, DTI )) :-
   eta( flight( POP,POT, DTI ), ATI ),
   ATI < 24.

print_time( HM ) :-
    Hours is floor( HM * 60 ) // 60,
    Minutes is floor( HM * 60 ) rem 60,
    min_boundry( Hours ),
    print(':'),
    min_boundry( Minutes ).


%====================================================================
% OD: Original Destination, NAS: Next Arrival Stop 
% apply 500 as flight speed, 
%--------------------------------------------------------------------
mph( OD, NAD, Duration ) :-
    airport( OD, _, Lat1, Lng1 ),
    airport( NAD, _, Lat2, Lng2 ),
    haversine( Lat1, Lng1, Lat2, Lng2, Distance),
    Duration is Distance / 500.

eta(flight(Term1, Term2, time( Hrs, Mins)), AT) :-
    mph( Term1, Term2, Duration ),
    ETE is Hrs + Mins / 60,
    AT is ETE + Duration. 


%====================================================================
% DCN: Departed City Name, ACN: Arrival City Name
% Using database, print departures and arrivals to console
% as long as the given inputs are valid 
%--------------------------------------------------------------------
writepath( [] ).

writepath( [flight(Depart, Arrive, time(Hrs, Mins))|List]) :-
    airport( Depart, DCN, _, _),
    airport( Arrive, ACN, _, _),
    hours( Hrs, Mins, DepartTime ),
    eta( flight(Depart, Arrive, time(Hrs, Mins)), Duration),
    write( '---------------------------------------------' ), nl,
    write('depart '), write('   '), write(Depart), write('   '),
    write(DCN), 
    write('  '),
    print_time( DepartTime ), nl,
    write('arrive '), write('   '), write(Arrive), write('   '),
    write(ACN),
    write('  '),  
    print_time( Duration ), nl,
    write( '---------------------------------------------'), nl,
    writepath( List ).
%--------------------------------------------------------------------

%====================================================================
% POP: Point of Presence, POT: Point of Termination, 
% NPOP: Next Point of Presence, ATI:Arrival Time Interval 
% LATI: Last Arrival Time Interval
% LDTI: Last Departure Time Interval 
% Make list of paths from POP to POT.
%--------------------------------------------------------------------

walkpath( POP, POT, [flight( POP, Approach, NPOP)|Outlist] ) :-
    not(POP = POT),
    flight(POP, Approach, NPOP),
    walkpath( Approach, POT, [flight( POP, Approach, NPOP)], Outlist ).

walkpath( POP, POP, _, [] ).

walkpath( POP, POT,
    [flight( Start, LDI, LDTI )| First],
    [flight( POP, Approach, NPOP )| List] ) :-
    flight( POP, Approach, NPOP),
    eta( flight( Start, LDI, LDTI ), LATI),
    thiry_min( LATI, NPOP ),
    max_airtime( flight( POP, Approach, NPOP )),
    Second = append( [flight( Start, LDI, LDTI )], First ),
    not( member( Approach, Second )),
    not( Approach = LDI ),
    walkpath( Approach, POT, 
    [flight( POP, Approach, NPOP )|Second], 
    List ).
%--------------------------------------------------------------------

%====================================================================
% Main function, calls walkpath, followed by nl, then writepath.
% Error: Departure == Arrival 
% Prolog NOT defined below
%--------------------------------------------------------------------
fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    walkpath( Depart, Arrive, List),
    !, nl,
    writepath( List ),
    true.

fly( Depart, Depart ) :-
    airport( Depart, DCN, _, _),  
    write( 'Invalid Input: '),
    write('Arrival and Destination airports are both ' ),
    write( DCN ),
    nl,
    write('Expected Input: fly(Depart, Arrive).'),
    nl,
    !, fail.

fly( Depart, _ ) :-
   not(airport(Depart, _, _, _)),
   nl,
   write(Depart),
   write(' is not a valid airport.'),
   nl,
   !, fail.

fly( _, Arrive ) :-
   not( airport(Arrive, _, _, _) ),
   nl,
   write(Arrive),
   write(' is not a valid airpot. '),
   nl,
   !, fail.

fly( _, _) :-
   write( ' Error: Airport does not exist in database' ),
   nl,
   !, fail.

%--------------------------------------------------------------------

