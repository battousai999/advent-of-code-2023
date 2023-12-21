using day_20_part_01;
using System.Text.RegularExpressions;

var nodeRegex = new Regex(@"^([%&]?\w+)\s->\s([\w,\s]+)$");

var rawModuleConfiguration = File.ReadAllLines("module-configuration.txt");

//var rawModuleConfiguration = (@"broadcaster -> a, b, c
//%a -> b
//%b -> c
//%c -> inv
//&inv -> a").Split(Environment.NewLine);

//var rawModuleConfiguration = (@"broadcaster -> a
//%a -> inv, con
//&inv -> b
//%b -> con
//&con -> output").Split(Environment.NewLine);

var builder = new ModuleConfigurationBuilder();

var moduleConfig = builder.Build(rawModuleConfiguration);

Console.WriteLine(moduleConfig.ToString());

var cycle = moduleConfig.CalculateCycle(1000);

Console.WriteLine();
Console.WriteLine(cycle.ToString());

var cycleLength = cycle.Sequences.Count;

var cyclesInThousand = 1000 / cycleLength;
var cyclesRemainder = 1000 % cycleLength;

var totalLowPulseCount = (cycle.TotalLowPulseCount * cyclesInThousand) + (cyclesRemainder == 0 ? 0 : cycle.Sequences[cyclesRemainder - 1].LowPulseCount);
var totalHighPulseCount = (cycle.TotalHighPulseCount * cyclesInThousand) + (cyclesRemainder == 0 ? 0 : cycle.Sequences[cyclesRemainder - 1].HighPulseCount);

var productOfPulses = totalLowPulseCount * totalHighPulseCount;

Console.WriteLine();
Console.WriteLine(productOfPulses.ToString());
