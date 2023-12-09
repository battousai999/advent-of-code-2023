// This was an attempt (in C#, as I'm more proficient in it than in F#) to make my day 8, part 2 solution
// faster (which it was), but still not fast enough.  
//
// I stopped it after running for several hours (getting up to 256,500,000,000 steps without finding the solution).

using System.Text.RegularExpressions;
using static Battousai.Utils.ConsoleUtils;

var nodeRegex = new Regex(@"(\w{3})\s=\s\((\w{3}),\s(\w{3})\)");

RunLoggingExceptions(() =>
{
    var rawDocuments = File.ReadAllLines("../day-08-part-01/documents.txt");

    var directions = rawDocuments
        .First()
        .Select(ch =>
        {
            return ch switch
            {
                'L' => Direction.Left,
                'R' => Direction.Right,
                _ => throw new ApplicationException($"Unexpected direction: {ch}")
            };
        })
        .ToArray();

    var graph = rawDocuments
        .Skip(2)
        .Select(line =>
        {
            var match = nodeRegex.Match(line);

            if (!match.Success)
                throw new ApplicationException($"Invalid node: {line}");

            return CreateNode(match.Groups[1].Value, match.Groups[2].Value, match.Groups[3].Value);
        })
        .ToDictionary(node => node.Id);

    var startingNodes = graph
        .Where(kvp => kvp.Value.IsStartingNode)
        .Select(kvp => kvp.Value)
        .ToArray();

    long FollowDirections(Dictionary<int, Node> graph, Node[] startingNodes, Direction[] directions)
    {
        var currentNodes = startingNodes.ToArray();
        var directionIndex = 0;
        var steps = 0L;

        while (!currentNodes.All(x => x.IsEndingNode))
        {
            var direction = directions[directionIndex];

            if (steps % 100000000 == 0)
                Log(steps.ToString());

            // Advance current nodes
            for (var i = 0; i < currentNodes.Length; i++)
            {
                var currentNode = currentNodes[i];

                if (direction == Direction.Left)
                    currentNodes[i] = graph[currentNode.LeftId];
                else
                    currentNodes[i] = graph[currentNode.RightId];
            }

            steps++;
            directionIndex = directionIndex == directions.Length - 1 ? 0 : directionIndex + 1;
        }

        return steps;
    }

    var results = FollowDirections(graph, startingNodes, directions);

    Log(results.ToString());
});

// Function to translate the "ABC" string form of node ids to a number
// (Since the % operator is a lot faster than string.EndsWith for determining
// an ending node—and the GetHashCode algorithm used by the Dictionary is also 
// probably faster for hasing ints than for hashing small strings.)
int TranslateNodeId(string nodeId)
{
    var ch1 = nodeId[0];
    var ch2 = nodeId[1];
    var ch3 = nodeId[2];

    var ord1 = ch1 - 'A';
    var ord2 = ch2 - 'A';
    var ord3 = ch3 - 'A';

    return ord1 * 26 * 26 + ord2 * 26 + ord3;
}

Node CreateNode(string id, string leftId, string rightId)
{
    var idValue = TranslateNodeId(id);
    var leftIdValue = TranslateNodeId(leftId);
    var rigthIdValue = TranslateNodeId(rightId);

    return new Node(idValue, leftIdValue, rigthIdValue);
}

public enum Direction
{
    Left,
    Right
}

public record Node(int Id, int LeftId, int RightId)
{
    public bool IsStartingNode => Id % 26 == 0;
    public bool IsEndingNode => Id % 26 == 25;
}
