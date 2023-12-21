using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static day_20_part_01.ModuleConfigurationBuilder;

namespace day_20_part_01;

public class ModuleConfiguration
{
    public record Pulse(string ModuleName, bool IsHighPulse, string FromModuleName);

    private readonly Dictionary<string, IModuleNode> modules;
    private readonly Queue<Pulse> pulseQueue;
    private readonly BroadcastModule broadcastModule;
    private readonly RxModule rxModule;

    private bool IsInStartingState => modules.Values.OfType<FlipFlopModule>().All(x => !x.IsOn);

    private bool WasRxModuleSignaledWithSingleLowPulse => rxModule.CapturedSingleLowPulse;

    public ModuleConfiguration(IEnumerable<ModuleDefinition> moduleDefinitions)
    {
        modules = moduleDefinitions
            .ToDictionary(
                x => x.Name,
                x =>
                {
                    var inputNames = moduleDefinitions
                        .Where(y => y.OutputNames.Contains(x.Name))
                        .Select(y => y.Name);

                    return (IModuleNode)(x.ModuleType switch
                    {
                        ModuleType.Broadcast => new BroadcastModule(this, x.Name, x.OutputNames),
                        ModuleType.FlipFlop => new FlipFlopModule(this, x.Name, x.OutputNames),
                        ModuleType.Conjunction => new ConjunctionModule(this, x.Name, x.OutputNames, inputNames),
                        _ => throw new ApplicationException($"invalid module type: {x.ModuleType}")
                    });
                });

        rxModule = new RxModule(this);

        modules.Add("rx", rxModule);

        pulseQueue = new Queue<Pulse>();

        broadcastModule = modules.Values.OfType<BroadcastModule>().FirstOrDefault() ?? throw new ApplicationException("no broadcast module defined");
    }

    public void ResetModules()
    {
        modules.Values.OfType<FlipFlopModule>().ToList().ForEach(x => x.Reset());
    }

    public void SendPulse(string moduleName, bool isHighPulse, string fromModuleName)
    {
        pulseQueue.Enqueue(new Pulse(moduleName, isHighPulse, fromModuleName));
    }

    public void PushButton()
    {
        pulseQueue.Enqueue(new Pulse(broadcastModule.Name, false, "button"));
    }

    public long CalculateCycle(int? maxIterations = null)
    {
        var lowPulseCount = 0;
        var highPulseCount = 0;
        long buttonCounter = 0;

        Sequence RunSequence()
        {
            rxModule.Reset();
            PushButton();
            buttonCounter++;

            var debugList = new List<Pulse>();

            while (pulseQueue.Count > 0)
            {
                var pulse = pulseQueue.Dequeue();

                debugList.Add(pulse);

                if (pulse.IsHighPulse)
                    highPulseCount++;
                else
                    lowPulseCount++;

                if (modules.TryGetValue(pulse.ModuleName, out var module))
                    module.Pulse(pulse.IsHighPulse, pulse.FromModuleName);
            }

            var flipFlopMap = String.Concat(modules.Values.OfType<FlipFlopModule>().Select(x => x.IsOn ? '1' : '0'));
            
            return new Sequence(lowPulseCount, highPulseCount, flipFlopMap);
        }

        Console.WriteLine();

        do
        {
            var sequence = RunSequence();

            Console.WriteLine($"{buttonCounter:#,##0} - RX {{ low = {rxModule.CapturedLowPulses}, high = {rxModule.CapturedHighPulses} }}");
        } while (!WasRxModuleSignaledWithSingleLowPulse);

        return buttonCounter;
    }

    public override string ToString()
    {
        var builder = new StringBuilder();

        builder.AppendLine("[");

        modules.Values.ToList().ForEach(x =>
        {
            builder.AppendLine($"    {x.GetType().Name}(Name = {x.Name}, Outputs = {String.Join(", ", x.OutputNames)})");
        });

        builder.AppendLine("]");

        return builder.ToString();
    }
}