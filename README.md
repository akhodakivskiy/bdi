To run the application:

    ./sbt run < src/test/resources/bdi.txt

To produce standalone jar (jar file is created here: `target/scala-2.10/bdi-transactor_2.10-1-one-jar.jar`):

    ./sbt oneJar

To run standalone jar:

    java -jar ./target/scala-2.10/bdi-transactor_2.10-1-one-jar.jar interview.Main < src/test/resources/bdi.txt
