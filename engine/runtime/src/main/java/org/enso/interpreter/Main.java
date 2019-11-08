package org.enso.interpreter;

import org.apache.commons.cli.*;
import org.enso.interpreter.runtime.RuntimeOptions;
import org.enso.interpreter.util.ScalaConversions;
import org.enso.pkg.Package;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

/** The main CLI entry point class. */
public class Main {
  private static final String RUN_OPTION = "run";
  private static final String HELP_OPTION = "help";
  private static final String NEW_OPTION = "new";

  /**
   * Builds the {@link Options} object representing the CLI syntax.
   *
   * @return an {@link Options} object representing the CLI syntax
   */
  private static Options buildOptions() {
    Option help = Option.builder("h").longOpt(HELP_OPTION).desc("Displays this message.").build();
    Option run =
        Option.builder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("file")
            .longOpt(RUN_OPTION)
            .desc("Runs a specified Enso file.")
            .build();

    Option newOpt =
        Option.builder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("path")
            .longOpt(NEW_OPTION)
            .desc("Creates a new Enso project.")
            .build();

    Options options = new Options();
    options.addOption(help).addOption(run).addOption(newOpt);
    return options;
  }

  /**
   * Prints the help message to the standard output.
   *
   * @param options object representing the CLI syntax
   */
  private static void printHelp(Options options) {
    new HelpFormatter().printHelp(Constants.LANGUAGE_ID, options);
  }

  /** Terminates the process with a failure exit code. */
  private static void exitFail() {
    System.exit(1);
  }

  /** Terminates the process with a success exit code. */
  private static void exitSuccess() {
    System.exit(0);
  }

  /**
   * Handles the {@code --new} CLI option.
   *
   * @param path root path of the newly created project
   */
  private static void createNew(String path) {
    Package.getOrCreate(new File(path));
    exitSuccess();
  }

  /**
   * Handles the {@code --run} CLI option.
   *
   * @param path path of the project or file to execute
   * @throws IOException when source code cannot be parsed
   */
  private static void run(String path) throws IOException {
    File file = new File(path);

    if (!file.exists()) {
      System.out.println("File " + file + " does not exist.");
      exitFail();
    }

    boolean projectMode = file.isDirectory();
    String packagePath = projectMode ? file.getAbsolutePath() : "";
    File mainLocation = file;
    if (projectMode) {
      Optional<Package> pkg = ScalaConversions.asJava(Package.fromDirectory(file));
      Optional<File> main = pkg.map(Package::mainFile);
      if (!main.isPresent() || !main.get().exists()) {
        System.out.println("Main file does not exist.");
        exitFail();
      }
      mainLocation = main.get();
    }

    Context context =
        Context.newBuilder(Constants.LANGUAGE_ID)
            .allowExperimentalOptions(true)
            .allowAllAccess(true)
            .option(RuntimeOptions.getPackagesPathOption(), packagePath)
            .out(System.out)
            .in(System.in)
            .build();
    Source source = Source.newBuilder(Constants.LANGUAGE_ID, mainLocation).build();
    context.eval(source);
    exitSuccess();
  }

  /**
   * Main entry point for the CLI program.
   *
   * @param args the command line arguments
   */
  public static void main(String[] args) throws IOException {
    Options options = buildOptions();
    CommandLineParser parser = new DefaultParser();
    CommandLine line;
    try {
      line = parser.parse(options, args);
    } catch (ParseException e) {
      printHelp(options);
      exitFail();
      return;
    }
    if (line.hasOption(HELP_OPTION)) {
      printHelp(options);
      exitSuccess();
      return;
    }
    if (line.hasOption(NEW_OPTION)) {
      createNew(line.getOptionValue(NEW_OPTION));
    }
    if (line.hasOption(RUN_OPTION)) {
      run(line.getOptionValue(RUN_OPTION));
    }

    printHelp(options);
    exitFail();
  }
}
