using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day_20_part_01;

public class ModuleConfigurationBuilder
{
    public enum ModuleType
    {
        Broadcast,
        FlipFlop,
        Conjunction
    }

    public class ModuleDefinition
    {
        public required string Name { get; set; }
        public required ModuleType ModuleType { get; set; }
        public required List<string> OutputNames { get; set; }
    }

    private static readonly Regex nodeRegex = new(@"^([%&]?\w+)\s->\s([\w,\s]+)$", RegexOptions.Compiled);

    public ModuleConfigurationBuilder()
    {
    }

    public ModuleConfiguration Build(string[] rawInput)
    {
        var moduleDefinitions = rawInput
            .Select(line =>
            {
                var match = nodeRegex.Match(line);

                if (!match.Success)
                    throw new ApplicationException($"invalid module line: {line}");

                var moduleName = match.Groups[1].Value;
                var rawOutputNames = match.Groups[2].Value;
                var outputNames = rawOutputNames.Split(",").Select(x => x.Trim()).ToList();

                var moduleType = moduleName switch
                {
                    var name when name.StartsWith('%') => ModuleType.FlipFlop,
                    var name when name.StartsWith('&') => ModuleType.Conjunction,
                    "broadcaster" => ModuleType.Broadcast,
                    _ => throw new ApplicationException($"invalid module type: {moduleName}")
                };

                if (moduleType != ModuleType.Broadcast)
                    moduleName = moduleName.Substring(1);

                return new ModuleDefinition { Name = moduleName, ModuleType = moduleType, OutputNames = outputNames };
            });

        var broadcastModule = moduleDefinitions.FirstOrDefault(x => x.ModuleType == ModuleType.Broadcast);

        if (broadcastModule == null)
            throw new ApplicationException("no broadcast module defined");

        return new ModuleConfiguration(moduleDefinitions);
    }
}
