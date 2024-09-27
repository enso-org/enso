package org.enso.tools.enso4igv;

import com.sun.jdi.connect.Connector;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Future;
import org.netbeans.api.debugger.jpda.JPDADebugger;
import org.netbeans.api.debugger.jpda.ListeningDICookie;
import org.netbeans.api.extexecution.ExecutionDescriptor;
import org.netbeans.api.extexecution.ExecutionService;
import org.netbeans.api.extexecution.base.ExplicitProcessParameters;
import org.netbeans.spi.project.ActionProvider;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;

import org.netbeans.api.extexecution.base.ProcessBuilder;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.spi.project.ActionProgress;
import org.openide.filesystems.FileUtil;
import org.openide.modules.Modules;
import org.openide.util.NbBundle;
import org.openide.util.NbPreferences;
import org.openide.util.RequestProcessor;
import org.openide.windows.IOProvider;

@NbBundle.Messages({
    "CTL_EnsoWhere=Enso Executable Location",
    "CTL_EnsoExecutable=enso/enso.bat",
    "# {0} - executable file",
    "MSG_CannotExecute=Cannot execute {0}",
    "# {0} - executable file",
    "# {1} - exit code",
    "MSG_ExecutionError=Process {0} finished with exit code {1}"
})
@ServiceProvider(service = ActionProvider.class)
public final class EnsoActionProvider implements ActionProvider {

    @Override
    public String[] getSupportedActions() {
        return new String[]{
            COMMAND_RUN, COMMAND_DEBUG,
            COMMAND_RUN_SINGLE, COMMAND_DEBUG_SINGLE
        };
    }

    @Override
    public void invokeAction(String action, Lookup lkp) throws IllegalArgumentException {
        var process = ActionProgress.start(lkp);
        var params = ExplicitProcessParameters.buildExplicitParameters(lkp);
        var fo = lkp.lookup(FileObject.class);
        var script = FileUtil.toFile(fo);

        var  dd = DialogDisplayer.getDefault();

        var prefs = NbPreferences.forModule(EnsoActionProvider.class);
        var exeKey = "enso.executable";

        var exe = prefs.get(exeKey, "");
        var nd = new NotifyDescriptor.InputLine(Bundle.CTL_EnsoExecutable(), Bundle.CTL_EnsoWhere());
        nd.setInputText(exe);

        var builderFuture = dd.notifyFuture(nd).thenApply(exec -> {
            var file = new File(exec.getInputText());
            if (file.canExecute()) {
                prefs.put(exeKey, file.getPath());

                var platform = JavaPlatform.getDefault();
                var isGraalVM = platform.findTool("native-image") != null;
                var java = platform.findTool("java");

                var b = ProcessBuilder.getLocal();
                b.setExecutable(file.getPath());
                b.setArguments(prepareArguments(script));
                b.setWorkingDirectory(script.getParent());
                b.setRedirectErrorStream(true);

                var env = b.getEnvironment();
                if (isGraalVM && isIGVConnected()) {
                    env.setVariable("JAVA_OPTS", "-Dgraal.Dump=Truffle:2");
                }
                var path = env.getVariable("PATH");
                if (path != null && java != null) {
                    var javaBinDir = FileUtil.toFile(java.getParent());
                    if (javaBinDir != null) {
                        var newPath = path + File.pathSeparator + javaBinDir;
                        env.setVariable("PATH", newPath);
                    }
                }

                return b;
            }
            var msg = Bundle.MSG_CannotExecute(file.getPath());
            throw new IllegalArgumentException(msg);
        });

        var waitForProcessFuture = builderFuture.thenCompose((builder) -> {
            var cf = new CompletableFuture<Integer>();
            var descriptor = new ExecutionDescriptor()
                .frontWindow(true).controllable(true)
                .controllable(true)
                .postExecution((exitCode) -> {
                    cf.complete(exitCode);
                });
            var launch = COMMAND_DEBUG_SINGLE.equals(action) || COMMAND_DEBUG.equals(action) ?
                new DebugAndLaunch(fo, builder, params) : builder;
            var service = ExecutionService.newService(launch, descriptor, script.getName());
            service.run();
            return cf;
        });

        waitForProcessFuture.thenAcceptBoth(builderFuture, (exitCode, builder) -> {
            boolean success;
            if (exitCode != null) {
                if (exitCode != 0) {
                    var msg = Bundle.MSG_ExecutionError(builder.getDescription(), exitCode);
                    var md = new NotifyDescriptor.Message(msg, NotifyDescriptor.ERROR_MESSAGE);
                    dd.notifyLater(md);
                    success = false;
                } else {
                    success = true;
                }
            } else {
                success = false;
            }
            process.finished(success);
        }).exceptionally((ex) -> {
            process.finished(false);
            if (ex instanceof CompletionException && ex.getCause() instanceof CancellationException) {
                return null;
            }
            dd.notifyLater(new NotifyDescriptor.Message(ex.getMessage(), NotifyDescriptor.ERROR_MESSAGE));
            return null;
        });
    }

    private static boolean isIGVConnected() {
        return Modules.getDefault().findCodeNameBase("org.graalvm.visualizer.connection") != null;
    }

    private static List<String> prepareArguments(File script) {
        var list = new ArrayList<String>();
        list.add("--run");
        list.add(script.getPath());
        return list;
    }

    @Override
    public boolean isActionEnabled(String action, Lookup lkp) throws IllegalArgumentException {
        return switch (action) {
            case COMMAND_RUN_SINGLE, COMMAND_DEBUG_SINGLE, COMMAND_RUN, COMMAND_DEBUG ->
                lkp.lookup(EnsoDataObject.class) != null;
            default -> false;
        };
    }

    static final class DebugAndLaunch implements Callable<Process> {
        private static final RequestProcessor RP = new RequestProcessor(DebugAndLaunch.class);
        private final FileObject script;
        private final ProcessBuilder builder;
        private final ExplicitProcessParameters params;
        private final Future<String> computeAddress;

        DebugAndLaunch(FileObject script, ProcessBuilder builder, ExplicitProcessParameters params) {
            this.script = script;
            this.builder = builder;
            this.computeAddress = RP.submit(this::initAddress);
            this.params = params;
    }

        @Override
        public Process call() throws Exception {
            var port = computeAddress.get();
            builder.getEnvironment().setVariable("JAVA_OPTS", "-agentlib:jdwp=transport=dt_socket,address=" + port);
            return builder.call();
        }

        private String initAddress() throws Exception {
            var lc = ListeningDICookie.create(-1);
            var connector = lc.getListeningConnector();

            var args = lc.getArgs();
            var address = connector.startListening(args);

            var properties = new HashMap<>();
            {
                var sourcePath = ClassPath.getClassPath(script, ClassPath.SOURCE);
                properties.put("sourcepath", sourcePath);
                properties.put("baseDir", FileUtil.toFile(script.getParent()));
                properties.put("name", script.getName());
            }

            var services = new Object[] { properties };
            int port = Integer.parseInt(address.substring(address.indexOf(':') + 1));
            Connector.IntegerArgument portArg = (Connector.IntegerArgument) args.get("port");
            portArg.setValue(port);

            RP.submit(() -> {
                JPDADebugger.startListening(connector, args, services);
                return null;
            });

            return Integer.toString(port);
        }
    }
}
