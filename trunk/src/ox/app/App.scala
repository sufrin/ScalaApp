package ox.app

/**
        <p>
        {{{ \$Id$ }}}
        
        An `App` is the core of a command-line application.  Its
        `Options` must be declared as a list of the `Opt`s that it
        is prepared to accept on the commandline.

        Its `Command` is the name it is known by.

        Its `Main` is invoked when the command-line options have
        been parsed.
        
        Most of its methods can be overridden.
        
        Every `Opt` is associated with a pattern and an
        arity (usually between 1 and 2) and a meaning 
        function. The subclasses of `Opt` defined
        here provide a useful selection 
        of meaning functions (and their associated arities).
        
        The list of command line arguments
        
        `arg1 arg2 ...`
        
        is parsed by finding a declared option whose
        pattern matches `arg1`, then taking `arity`
        arguments (including `arg1` and passing them to the
        meaning function. Then `arity` arguments are removed from
        the list, and the parsing continues. The parsing stops when
        there are no arguments left (or when a parsing error occurs).        

        <p>
        See `AppTest.scala` for a simple and effective way of
        using this package to accumulate a queue of jobs to
        be performed by the program (and environments in which
        the jobs are to be performed). There
        the Job queue is available when the options and paths
        have all been parsed -- but not before. An error in
        parsing causes the program to exit '''before any semantic processing
        has been done.'''
*/
abstract class App
{ import App._

  val Options:  List[Opt]
  val Command:  String
  def Main():     Unit
  
  /**
    Entry point to an `App` run from the command line: 
    parses the command-line options and
    then runs `Main`.
  */
  def main(args: Array[String]): Unit = 
  {  
     main(args.toList)
     Main
  }
  
  /**
    The command-line parser. It analyses `args` according to
    the notation/semantics defined in `Options`.
  */
  def main(args: List[String]):  Unit = 
  { var remaining = args
    while (remaining.nonEmpty)
    { val arg = remaining.head
      Options.find((cmd => arg.matches(cmd.pat))) match
      { case Some(cmd: Opt) => 
          if (cmd.arity<0)
          {
            cmd.meaning(remaining)
            remaining = Nil
          }
          else
          if (cmd.arity<=remaining.length)
          { val local = remaining.take(cmd.arity)
            try   { cmd.meaning(local); remaining = remaining.drop(cmd.arity)}
            catch { case _: OptFail  => Unacceptable(cmd, local, args); Fail }            
          }
          else
          {
             NotEnough(cmd, args); Fail
          }
        case None      => Invalid(arg, args); Fail
      }
    }
  }
  
  /** Invoked after an error in the command-line arguments has been
      reported. 
  */
  def Fail(): Unit = System.exit(2)
  
  /**
      An argument sequence (`local`) that appears to satisfy the
      `cmd` option pattern cannot actually be parsed --
      usually because a subsidiary or subsequent argument 
      fails to match a constraint. A report is made.
  */
  def Unacceptable(cmd: Opt, local: List[String], args: List[String]): Unit =
  { Console.err.print("%s ".format(Command))
    for (arg<-args) Console.err.print("%s ".format(arg))
    Console.err.println()
    var l = ""
    for (arg<-local) l = l+arg+" "
    Console.err.println("Invalid argument(s) %s(%s %s)".format(l, cmd.flag, cmd.help))
  }
  
  /**
      An argument sequence that appears to satisfy the
      `cmd` option pattern cannot actually be parsed --
      usually because there aren't enough arguments available.
  */  
  def NotEnough(cmd: Opt, args: List[String]): Unit =
  { Console.err.print("%s ".format(Command))
    for (arg<-args) Console.err.print("%s ".format(arg))
    Console.err.println()
    Console.err.println("Not enough arguments for: %s %s".format(cmd.flag, cmd.help))
  }
  
  /**
    No candidate option pattern can be found that matches the 
    (head of) the given argument sequence.
  */
  def Invalid(arg: String, args: List[String]): Unit =
  { Console.err.println("Invalid: "+arg)
    Console.err.print("%s ".format(Command))
    for (arg<-args) Console.err.print("%s ".format(arg))
    Console.err.println()
    Usage 
  }
  
  private lazy val flagLength : Int =
  { var v = 0
    for (cmd<-Options) if (cmd.flag.length>v) { v = cmd.flag.length }
    v
  }
  
  def Usage(): Unit =
  { Console.err.println("Usage: %s [args] -- where an [arg] is one of:".format(Command))
    //noinspection ScalaMalformedFormatString
    for (cmd<- Options) Console.err.println(s"%${flagLength}s %s".format(cmd.flag, cmd.help))
  }
  
}

object App 
{  
 
/**
        Root class of option syntax descriptors.
*/
abstract class Opt(_pat: String, _help: Seq[String])
{ /**
    The intended effect of a sequence of arguments that starts with 
    a string that matches `pat`.
  */
  val meaning: List[String] => Unit
  /**
    The number of arguments (including the starting argument) that
    are consumed by this option. Negative `arity` consumes
    all remaining arguments.
  */
  val arity:   Int
  private
  val litPat = _pat.startsWith("'")
  /**
    The pattern that is matched against the starting argument
    of a sequence. If `_pat` started with a single quote mark ('),
    then the rest of the pattern is taken literally, otherwise
    the pattern is in the `jdk.util.regex.Pattern` notation.
  */  
  val pat: String = if (litPat) App.Quote(_pat.substring(1)) else _pat
  
  /* The help string to be used by the standard `Usage` and
     error-message methods if this option is used erroneously.
  */
  lazy val help: String = _help.length match
                  { case 0 => ""
                    case 1 => _help.head
                    case 2 => _help.head
                  }
  /**
     The pattern text to be used by the standard `Usage` and
     error-message methods. The same as `pat` unless `pat` is
     to be taken literally because `_pat` started with a (').
  */
  lazy val flag: String = _help.length match
                  { case 0 => if (litPat) _pat.substring(1) else pat
                    case 1 => if (litPat) _pat.substring(1) else pat
                    case 2 => _help.last
                  }
  /**
        Throws an `OptFail` exception to the option parser if
        `arg` doesn't match `pat`. 
  */                
  def Allow(arg: String, pat: String): Unit =
     if (!arg.matches(pat)) throw new OptFail 
  
}

/**
     An option that takes no arguments, and runs `effect` when  it's parsed.
*/
class Flag(_pat: String, effect: => Unit, _help: Seq[String]) extends Opt(_pat, _help) {
     val arity = 1
     val meaning: List[String]=>Unit = { case _ :: _ => effect; case Nil =>}
}

/**
     An option that takes a single argument, `arg` and runs `effect(arg)` when  it's parsed.
*/
class Arg(_pat: String, effect: String => Unit, _help: Seq[String]) extends Opt(_pat,_help) {
     val arity= 2
     val meaning: List[String]=>Unit = { case _ :: arg :: _ => effect(arg)
                                         case _ =>
     }
}

/**
     An option that takes a single argument, `arg` and runs `effect(arg)` 
     when  it's parsed. The argument must not start with "-".
*/
class PathArg(_pat: String, effect: String => Unit, _help: Seq[String]) extends Opt(_pat,_help) {
     val arity= 2
     val meaning: List[String]=>Unit = 
     { case _ :: arg :: _ => Allow(arg, "[^-].*"); effect(arg)
     case _ =>
     }
}

/**
     An option that takes a single Int argument, `arg` and runs `effect(arg)` 
     when  it's parsed. The argument must have the right form.
*/
class Int32(_pat: String, effect: Int => Unit, _help: Seq[String]) extends Opt(_pat,_help) {
     val arity= 2
     val meaning: List[String]=>Unit = 
         { case _ :: arg :: _ => Allow(arg, "-?[0-9]+"); effect(arg.toInt)
         case _ =>
         }
}



/**
     An option that takes a single floating point argument, `arg` and runs `effect(arg)` 
     when  it's parsed. The argument must have the right form.
*/
class Real(_pat: String, effect: Double => Unit, _help: Seq[String]) extends Opt(_pat,_help) {
     val arity= 2
     val meaning: List[String]=>Unit = 
         { case _ :: arg :: _ => Allow(arg, "-?([0-9]+)?([.][0-9]+)?([Ee]-?[0-9]+)?"); effect(arg.toDouble)
         case _ =>
         }
}

/**
     An option that takes a single Long argument, `arg` and runs `effect(arg)` 
     when  it's parsed. The argument must have the right form.
*/
class Int64(_pat: String, effect: Long => Unit, _help: Seq[String]) extends Opt(_pat,_help) {
     val arity = 2
     val meaning: List[String]=>Unit = 
         { case _ :: arg :: _ => Allow(arg, "(-?[0-9]+)"); effect(arg.toLong)
         case _ =>
         }
}

/**
        An (optionless) argument, `arg` that must not start with "-", and runs
        `effect(arg)` when  parsed.
*/
class Path(_pat: String, effect: String => Unit, _help: Seq[String]) extends Opt(_pat,_help) {
     val arity = 1
     val meaning: List[String]=>Unit = 
         { case arg :: _ => Allow(arg, "[^-].*"); effect(arg)
         case _ =>
         }
}

/**
        An (optionless) argument, `arg` that runs
        `effect(arg)` when  parsed.
*/
class Lit(_pat: String, effect: String => Unit, _help: Seq[String]) extends Opt(_pat,_help) {
     val arity = 1
     val meaning: List[String]=>Unit = 
         { case arg :: _ => effect(arg)
         case _ =>
         }
}

/**
        An option that causes all the remaining arguments to be
        passed, as a list, to `effect`.
*/
class Rest(_pat: String, effect: List[String] => Unit, _help: Seq[String]) extends Opt(_pat,_help) {
     val arity: Int = -1
     val meaning: List[String]=>Unit = (args => effect(args.tail))
}

class OptFail extends Exception {}

/**
        Convenience methods that construct new `Opt`ions of the
        appropriate kinds.
*/
  
   
   type App = ox.app.App

   def Quote(s: String): String = java.util.regex.Pattern.quote(s)

   def Flag (_pat: String, effect: => Unit, _help: String*) = 
            new Flag (_pat, effect, _help)
   def Arg  (_pat: String, effect: String => Unit, _help: String*) = 
            new Arg(_pat, effect, _help)
   def PathArg (_pat: String, effect: String => Unit, _help: String*) = 
               new PathArg(_pat, effect, _help)
   def Int32(_pat: String, effect: Int => Unit, _help: String*) = 
            new Int32(_pat, effect, _help)
   def Int64(_pat: String, effect: Long => Unit, _help: String*) = 
            new Int64(_pat, effect, _help)
   def Real(_pat: String, effect: Double => Unit, _help: String*) = 
            new Real(_pat, effect, _help)   
   def Path(_pat: String, effect: String => Unit, _help: String*) = 
           new Path(_pat, effect, _help)
   def Lit(_pat: String, effect: String => Unit, _help: String*) = 
           new Lit(_pat, effect, _help)
   def Rest(_pat: String, effect: List[String] => Unit, _help: String*) = 
           new Rest(_pat, effect, _help)
}







