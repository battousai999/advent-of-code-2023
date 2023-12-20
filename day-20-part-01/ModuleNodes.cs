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

    void Pulse(bool isLowPulse);
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

    public void Pulse(bool isLowPulse)
    {
        OutputNames.ForEach(name => Config.SendPulse(name, isLowPulse));
    }
}

public class FlipFlopModule : BaseModule, IModuleNode
{
    public bool IsOn { get; private set; } = false;

    public FlipFlopModule(ModuleConfiguration config, string name, List<string> outputNames) : base(config, name, outputNames)
    {

    }

    public void Pulse(bool isLowPulse)
    {
        if (!isLowPulse)
            return;

        var wasOn = IsOn;

        IsOn = !IsOn;

        if (wasOn)
            OutputNames.ForEach(name => Config.SendPulse(name, true));
        else
            OutputNames.ForEach(name => Config.SendPulse(name, false));
    }
}

public class ConjunctionModule : BaseModule, IModuleNode
{
    public ConjunctionModule(ModuleConfiguration config, string name, List<string> outputNames) : base (config, name, outputNames)
    {

    }

    public void Pulse(bool isLowPulse)
    {
        // TODO: Implement...
    }
}
