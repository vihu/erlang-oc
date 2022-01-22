-module(erlang_oc_genetic).

-export([samples/1, new_sample/5]).
-export([check/1, q/1, iterations/1, flips/1, epsilon/1]).

-export([start/0, start/2]).

-export([
    generate_genome/1,
    generate_population/2,
    single_point_crossover/2,
    mutation/3,
    fitness/4,
    selection_pair/2,
    from_genome/2
]).

-export([weighted_random/2, count/2]).

-record(sample, {
    check :: boolean(),
    epsilon :: float(),
    flips :: non_neg_integer(),
    iterations :: non_neg_integer(),
    q :: pos_integer()
}).

-type sample() :: #sample{}.
-type genome() :: [pos_integer()].
-type population() :: [genome()].

start() ->
    Samples = samples("priv/eqc.json"),
    main(Samples, 10000, 5).

start(FitnessLimit, GenerationLimit) ->
    Samples = samples("priv/eqc.json"),
    main(Samples, FitnessLimit, GenerationLimit).

-spec generate_genome(Length :: pos_integer()) -> genome().
generate_genome(Length) ->
    [rand_from_list([0, 1]) || _ <- lists:seq(1, Length)].

-spec generate_population(Size :: pos_integer(), GenomeLength :: pos_integer()) -> population().
generate_population(Size, GenomeLength) ->
    [generate_genome(GenomeLength) || _ <- lists:seq(1, Size)].

-spec fitness(
    Genome :: genome(),
    Samples :: [sample()],
    IterationLimit :: pos_integer(),
    MaxFlips :: non_neg_integer()
) -> pos_integer().
fitness(Genome, Samples, IterationLimit, MaxFlips) ->
    case length(Genome) /= length(Samples) of
        true ->
            erlang:throw({error, genome_samples_length_mismatch});
        false ->
            ok
    end,

    try
        Value =
            lists:foldl(
                fun({Index, Sample}, Acc) ->
                    case lists:nth(Index, Genome) of
                        1 ->
                            Data = crypto:strong_rand_bytes(8),
                            StreamId = 0,
                            BlockSize = 4,
                            {Check1, Iterations1, Flips1, _Header, _BestMatch} =
                                erlang_oc_util:execute(
                                    Data,
                                    epsilon(Sample),
                                    q(Sample),
                                    BlockSize,
                                    StreamId,
                                    MaxFlips
                                ),
                            %% io:format(user, "Data: ~p, Check1: ~p~n", [Data, Check1]),

                            C1 = Check1 == true orelse check(Sample) == true,
                            C2 = Iterations1 < IterationLimit,
                            C3 = Flips1 > MaxFlips,

                            case C1 of
                                false ->
                                    throw({break, failed});
                                true ->
                                    case {C2, C3} of
                                        {true, true} ->
                                            %% io:format(user, "Sample: ~p, 100 points!~n", [Sample]),
                                            100 + Acc;
                                        {true, false} ->
                                            %% io:format(user, "Sample: ~p, 50 points!~n", [Sample]),
                                            50 + Acc;
                                        {false, true} ->
                                            %% io:format(user, "Sample: ~p, 10 points!~n", [Sample]),
                                            10 + Acc;
                                        {false, false} ->
                                            %% io:format(user, "Sample: ~p, 0 points!~n", [Sample]),
                                            0 + Acc
                                    end
                            end;
                        0 ->
                            Acc
                    end
                end,
                0,
                lists:zip(lists:seq(1, length(Samples)), Samples)
            ),
        Value
    catch
        throw:_Ret ->
            0
    end.

-spec selection_pair(Population :: population(), FitnessFun :: fun()) -> population().
selection_pair(Population, FitnessFun) ->
    WeightedPop = [{Genome, FitnessFun(Genome)} || Genome <- Population],
    weighted_random(WeightedPop, 2).

-spec single_point_crossover(A :: genome(), B :: genome()) -> {genome(), genome()}.
single_point_crossover(A, B) ->
    case length(A) /= length(B) of
        true ->
            erlang:throw({error, genome_a_b_mismatch});
        false ->
            ok
    end,

    Length = length(A),

    case Length < 2 of
        true ->
            {A, B};
        false ->
            P = rand:uniform(Length - 1),
            {
                lists:sublist(A, P) ++ lists:sublist(B, P + 1, length(B)),
                lists:sublist(B, P) ++ lists:sublist(A, P + 1, length(A))
            }
    end.

-spec mutation(Genome :: genome(), Num :: pos_integer(), Probability :: float()) -> genome().
mutation(Genome, Num, Probability) ->
    case rand:uniform() > Probability of
        false ->
            lists:foldl(
                fun(_, Acc) ->
                    Index = rand:uniform(length(Genome)),
                    lists:sublist(Acc, Index - 1) ++
                        [lists:nth(Index, Acc) bxor 1] ++
                        lists:sublist(Acc, Index + 1, length(Genome))
                end,
                Genome,
                lists:seq(1, Num)
            );
        true ->
            Genome
    end.

-spec run_evolution(
    PopulateFun :: fun(),
    FitnessFun :: fun(),
    SelectionFun :: fun(),
    CrossOverFun :: fun(),
    MutationFun :: fun(),
    FitnessLimit :: pos_integer(),
    GenerationLimit :: pos_integer()
) -> {population(), pos_integer()}.
run_evolution(
    PopulateFun,
    FitnessFun,
    SelectionFun,
    CrossOverFun,
    MutationFun,
    FitnessLimit,
    GenerationLimit
) ->
    Population = PopulateFun(),

    try
        lists:foldl(
            fun(Gen, {GenAcc, PopAcc}) ->
                io:format(user, "Gen: ~p~n", [Gen]),
                Pop0 = lists:sort(
                    fun(G1, G2) ->
                        FitnessFun(G1) > FitnessFun(G2)
                    end,
                    PopAcc
                ),

                case FitnessFun(hd(Pop0)) >= FitnessLimit of
                    true ->
                        throw({break, {GenAcc, Pop0}});
                    false ->
                        [First, Second | _] = Pop0,
                        NextGen0 = [First, Second],

                        NextGen = lists:foldl(
                            fun(_I, Acc) ->
                                [FirstP, SecondP | _Parents] = SelectionFun(Pop0, FitnessFun),
                                {OffspringA0, OffspringB0} = CrossOverFun(FirstP, SecondP),
                                OffspringA = MutationFun(OffspringA0),
                                OffspringB = MutationFun(OffspringB0),
                                Next = Acc ++ [OffspringA, OffspringB],
                                Next
                            end,
                            NextGen0,
                            lists:seq(1, round(length(Pop0) / 2) - 1)
                        ),

                        {
                            GenAcc + 1,
                            lists:sort(
                                fun(G1, G2) ->
                                    FitnessFun(G1) > FitnessFun(G2)
                                end,
                                NextGen
                            )
                        }
                end
            end,
            {0, Population},
            lists:seq(1, GenerationLimit)
        )
    catch
        throw:{break, {GenAcc, PopAcc}} ->
            {GenAcc, lists:sort(fun(G1, G2) -> FitnessFun(G1) > FitnessFun(G2) end, PopAcc)}
    end.

main(Samples, FitnessLimit, GenerationLimit) ->
    PopulateFun = fun() -> generate_population(10, length(Samples)) end,
    FitnessFun = fun(Genome) -> fitness(Genome, Samples, 100, 0) end,
    SelectionFun = fun selection_pair/2,
    MutationFun = fun(Genome) -> mutation(Genome, 1, 0.5) end,
    CrossOverFun = fun single_point_crossover/2,
    {Generations, Population} = run_evolution(
        PopulateFun,
        FitnessFun,
        SelectionFun,
        CrossOverFun,
        MutationFun,
        FitnessLimit,
        GenerationLimit
    ),

    Genome = hd(Population),
    Res = from_genome(Genome, Samples),
    Top10 = lists:sublist(
        lists:sort(
            fun(A, B) ->
                maps:get(flips, A) > maps:get(flips, B) andalso
                    maps:get(iterations, A) =< maps:get(iterations, B)
            end,
            Res
        ),
        10
    ),

    io:format(user, "generations: ~p~ntop10: ~p~nall: ~p~n", [Generations, Top10, Res]),
    {Generations, Top10}.

%% Helper functions for getting random item from list, and weighted selection

rand_from_list(List) ->
    lists:nth(rand:uniform(length(List)), List).

to_cdf(WeightTuples) ->
    lists:foldl(fun cdf_trans/2, {[], [0]}, WeightTuples).

cdf_trans({Item, Wt}, {Items, Wts}) when Wt > 0, is_integer(Wt) ->
    {[Item | Items], [hd(Wts) + Wt | Wts]};
cdf_trans(_, R) ->
    R.

match_item({[Item | Items], [Sum | Sums]}, Num) when is_number(Num), Num > 0 ->
    case Num > Sum of
        true ->
            Item;
        false ->
            match_item({Items, Sums}, Num)
    end.

random_cdf({Items, [0 | Weights]}, N) ->
    random_cdf({Items, Weights}, N);
random_cdf({Items, [Total | Weights]}, N) ->
    [match_item({Items, Weights}, rand:uniform(Total)) || _ <- lists:seq(1, N)].

weighted_random(WeightTuples, N) ->
    random_cdf(to_cdf(WeightTuples), N).

count(Needle, Haystack) -> count(Needle, Haystack, 0).
count(_, [], Count) -> Count;
count(X, [X | Rest], Count) -> count(X, Rest, Count + 1);
count(X, [_ | Rest], Count) -> count(X, Rest, Count).

-spec from_genome(Genome :: genome(), Samples :: [sample()]) -> [sample()].
from_genome(Genome, Samples) ->
    lists:foldl(
        fun({Index, Sample}, Acc) ->
            case lists:nth(Index, Genome) == 1 of
                true ->
                    [
                        #{
                            q => q(Sample),
                            epsilon => epsilon(Sample),
                            iterations => iterations(Sample),
                            flips => flips(Sample)
                        }
                        | Acc
                    ];
                false ->
                    Acc
            end
        end,
        [],
        lists:zip(lists:seq(1, length(Samples)), Samples)
    ).

samples(SamplePath) ->
    {ok, Data} = file:read_file(SamplePath),
    Map = jsx:decode(Data),
    Samples0 = maps:get(<<"data">>, Map),

    lists:map(
        fun(Sample) ->
            Check = maps:get(<<"check">>, Sample),
            Epsilon = maps:get(<<"epsilon">>, Sample),
            Flips = maps:get(<<"flips">>, Sample),
            Q = maps:get(<<"q">>, Sample),
            Iterations = maps:get(<<"iterations">>, Sample),
            new_sample(Check, Epsilon, Flips, Iterations, Q)
        end,
        Samples0
    ).

-spec new_sample(
    Check :: boolean(),
    Epsilon :: float(),
    Flips :: non_neg_integer(),
    Iterations :: non_neg_integer(),
    Q :: pos_integer()
) -> sample().
new_sample(Check, Epsilon, Flips, Iterations, Q) ->
    #sample{
        check = Check,
        epsilon = Epsilon,
        flips = Flips,
        iterations = Iterations,
        q = Q
    }.

check(Sample) -> Sample#sample.check.
epsilon(Sample) -> Sample#sample.epsilon.
flips(Sample) -> Sample#sample.flips.
iterations(Sample) -> Sample#sample.iterations.
q(Sample) -> Sample#sample.q.
