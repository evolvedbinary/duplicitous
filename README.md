Duplicitous
===========

A simple tool for replacing sensitive text within documents without changing the document structure.

Currently supports XML documents, other formats such as JSON could be easily added in future.


Building
--------
* Requires: `sbt 0.13.6+`

If you want an *assembly* for running from the command line:
```bash
sbt assembly
```

If you want a *package* for including in your own Java/Scala/JVM project:
```bash
sbt make-pom package
```

The resulting files are in `target/scala-2.11`.


**..or** if you want to install an artifact to your local Maven repsository:

```bash
sbt publishM2
```

If you use Ivy, you can use `publishLocal` instead of `publishM2`.


Running
-------
After building or obtaining an assembly:

```bash
$ java -cp target/scala-2.11/duplicitous-assembly-1.0.jar com.evolvedbinary.xml.duplicitous.DuplicitousApp --help
```
