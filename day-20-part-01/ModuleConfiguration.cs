using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static day_20_part_01.ModuleConfigurationBuilder;

namespace day_20_part_01;

public class ModuleConfiguration
{
    private readonly Dictionary<string, IModuleNode> modules;
    private readonly BroadcastModule broadcastModule;

    public ModuleConfiguration(IEnumerable<ModuleDefinition> moduleDefinitions)
    {
        modules = moduleDefinitions
            .ToDictionary(
                x => x.Name,
                x =>
                {
                    return (IModuleNode)(x.ModuleType switch
                    {
                        ModuleType.Broadcast => new BroadcastModule(this, x.Name, x.OutputNames),
                        ModuleType.FlipFlop => new FlipFlopModule(this, x.Name, x.OutputNames),
                        ModuleType.Conjunction => new ConjunctionModule(this, x.Name, x.OutputNames),
                        _ => throw new ApplicationException($"invalid module type: {x.ModuleType}")
                    });
                });

        broadcastModule = modules.Values.OfType<BroadcastModule>().FirstOrDefault() ?? throw new ApplicationException("no broadcast module defined");
    }

    public void SendPulse(string moduleName, bool isLowPulse)
    {
        // TODO: Queue the sending of the pulse...
    }

    public void PushButton()
    {
        // TODO: Implement (send low pulse to the broadcast module)
    }

    public override string ToString()
    {
        var builder = new StringBuilder();

        builder.AppendLine("[");

        modules.Values.ToList().ForEach(x =>
        {
            builder.AppendLine($"    {x.GetType().Name}(Name = {x.Name}, Outputs = {String.Join(", ", x.OutputNames)}");
        });

        builder.AppendLine("]");

        return builder.ToString();
    }
}