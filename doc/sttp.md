## Using as an sttp client

Add the dependency:

```scala
"com.softwaremill.sttp.tapir" %% "tapir-sttp-client" % "0.12.0"
```

To make requests using an endpoint definition using the [sttp client](https://github.com/softwaremill/sttp), import:

```scala
import sttp.tapir.client.sttp._
```

This adds the `toSttpRequest(Uri)` extension method to any `Endpoint` instance which, given the given base URI returns a 
function:

```scala
I => Request[Either[E, O], Nothing]
```

Note that this is a one-argument function, where the single argument is the input of end endpoint. This might be a 
single type, a tuple, or a case class, depending on the endpoint description. 

After providing the input parameters, a description of the request to be made is returned. This can be further 
customised and sent using any sttp backend.

See  the [runnable example](https://github.com/softwaremill/tapir/blob/master/examples/src/main/scala/sttp/tapir/examples/BooksExample.scala)
for example usage.
