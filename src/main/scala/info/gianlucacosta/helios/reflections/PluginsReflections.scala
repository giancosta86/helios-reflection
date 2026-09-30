package info.gianlucacosta.helios.reflections

import java.io.File
import java.net.URLClassLoader

import org.reflections.Reflections
import org.reflections.util.{ClasspathHelper, ConfigurationBuilder}


object PluginsReflections {
  /**
    * Creates a Reflections object for:
    *
    * @param internalRootPackage    The given root package, which is considered <i>internal</i> - that is,
    *                               available even when the plugins directory is missing or with no files
    * @param pluginsDirectoryOption an optional (flat) directory containing plugin JAR files
    *
    */
  def apply(internalRootPackage: String, pluginsDirectoryOption: Option[File]): Reflections = {
    val externalJarFiles: Array[File] =
      pluginsDirectoryOption
        .map(externalDirectory => {
          if (externalDirectory.isDirectory)
            externalDirectory
              .listFiles()
              .filter(_.getName.toLowerCase.endsWith(".jar"))
          else
            Array[File]()
        })
        .getOrElse(
          Array[File]()
        )

    val reflectionsClassLoader = new URLClassLoader(
      externalJarFiles.map(_.toURI.toURL),
      getClass.getClassLoader
    )

    val configuration =
      new ConfigurationBuilder()
        .addClassLoader(reflectionsClassLoader)
        .addUrls(ClasspathHelper.forPackage(internalRootPackage))
        .addUrls(externalJarFiles.map(_.toURI.toURL): _*)

    new Reflections(configuration)
  }
}
