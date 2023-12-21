using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace day_20_part_01;

public class Cycle
{
    public List<Sequence> Sequences { get; private set; } = new List<Sequence>();

    public int TotalLowPulseCount => Sequences.LastOrDefault()?.LowPulseCount ?? 0;
    public int TotalHighPulseCount => Sequences.LastOrDefault()?.HighPulseCount ?? 0;

    public void AddSequence(Sequence sequence)
    {
        Sequences.Add(sequence);
    }

    public override string ToString()
    {
        var builder = new StringBuilder();

        builder.AppendLine("{");

        builder.AppendLine($"    TotalLowPulses: {TotalLowPulseCount}, TotalHighPulses: {TotalHighPulseCount},");
        builder.AppendLine($"    Sequences ({Sequences.Count}): [");

        Sequences.ForEach(x => builder.AppendLine($"        {x},"));

        builder.AppendLine("    ]");
        builder.AppendLine("}");

        return builder.ToString();
    }
}

public class Sequence
{
    public int LowPulseCount { get; private set; }
    public int HighPulseCount { get; private set; }
    public string FlipFlopMap { get; private set; }

    public Sequence(int lowPulseCount, int highPulseCount, string flipFlopMap)
    {
        LowPulseCount = lowPulseCount;
        HighPulseCount = highPulseCount;
        FlipFlopMap = flipFlopMap;
    }

    override public string ToString()
    {
        return $"{{ LowPulses: {LowPulseCount}, HighPulses: {HighPulseCount}, FlipFlopMap = {FlipFlopMap} }}";
    }
}