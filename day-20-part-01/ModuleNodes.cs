using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace day_20_part_01;

public interface IModuleNode
{
    string Name { get; }
    List<string> OutputNames { get; }

    void Pulse(bool isHighPulse, string fromModuleName);
}

public abstract class BaseModule
{
    public ModuleConfiguration Config { get; private set; }
    public string Name { get; private set; }
    public List<string> OutputNames { get; private set; }

    public BaseModule(ModuleConfiguration config, string name, List<string> outputNames)
    {
        Config = config;
        Name = name;
        OutputNames = outputNames;
    }
}

public class BroadcastModule : BaseModule, IModuleNode
{
    public BroadcastModule(ModuleConfiguration config, string name, List<string> outputNames) : base(config, name, outputNames)
    {
    }

    public void Pulse(bool isHighPulse, string fromModuleName)
    {
        OutputNames.ForEach(name => Config.SendPulse(name, isHighPulse, Name));
    }
}

public class FlipFlopModule : BaseModule, IModuleNode
{
    public bool IsOn { get; private set; } = false;

    public FlipFlopModule(ModuleConfiguration config, string name, List<string> outputNames) : base(config, name, outputNames)
    {

    }

    public void Pulse(bool isHighPulse, string fromModuleName)
    {
        if (isHighPulse)
            return;

        var wasOn = IsOn;

        IsOn = !IsOn;

        if (wasOn)
            OutputNames.ForEach(name => Config.SendPulse(name, false, Name));
        else
            OutputNames.ForEach(name => Config.SendPulse(name, true, Name));
    }
}

public class ConjunctionModule : BaseModule, IModuleNode
{
    private readonly List<string> inputNames;
    private readonly bool[] pulseMemory;    // false = low, true = high

    public ConjunctionModule(ModuleConfiguration config, string name, List<string> outputNames, IEnumerable<string> inputNames) : base (config, name, outputNames)
    {
        this.inputNames = inputNames.ToList();
        pulseMemory = inputNames.Select(_ => false).ToArray();
    }

    public void Pulse(bool isHighPulse, string fromModuleName)
    {
        var index = inputNames.IndexOf(fromModuleName);
        pulseMemory[index] = isHighPulse;

        if (pulseMemory.All(x => x))
            OutputNames.ForEach(name => Config.SendPulse(name, false, Name));
        else
            OutputNames.ForEach(name => Config.SendPulse(name, true, Name));
    }
}
