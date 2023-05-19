<p align="center">
<img src="https://raw.githubusercontent.com/typedbyte/hagato/main/logo.png" alt="hagato" title="hagato"/>
</p>

# Haskell Gamedev Toolkit

`hagato` is a Haskell library for developing games from scratch. It is not a game engine, but a collection of loosely coupled, easily combinable sub-libraries which can be used or ignored as desired, thus allowing developers to select features and technologies at will while remaining in full control of the overall game architecture. Some sub-libraries are thin integration layers over existing libraries, some are developed from scratch. The following sub-libraries are currently available, which can be imported individually:

* [`hagato:with-core`](/src/hagato-core): types and functions for animations, math, camera, input, logging and game loop handling.
* [`hagato:with-glfw`](/src/hagato-glfw): Vulkan-based window handling using the GLFW libary.
* [`hagato:with-gltf`](/src/hagato-gltf): loader for the GLTF format to import 3D scenes (for Blender integration, for example).
* [`hagato:with-gltf-vulkan`](/src/hagato-gltf-vulkan): interop between GLTF and Vulkan.
* [`hagato:with-vma`](/src/hagato-vma): Vulkan-based memory handling using the Vulkan Memory Allocator (VMA) library.
* [`hagato:with-vulkan`](/src/hagato-vulkan): thin layer over the Vulkan API which offers features like constraint-based device and queue selection, virtual frames, recreateable entities, a descriptor set allocator and Vulkan-compatible camera handling and ray casting.

Many libraries have a companion sub-library with the name suffix [`*-effectful`](/src) which provides an easy integration with the [effectful](https://hackage.haskell.org/package/effectful-core) ecosystem. None of the libraries can be considered complete, but they cover many common use cases and it is intended to evolve them on the go when actually developing games. Since the libraries are work in progress and Haddock documentation is still missing, a Hackage release is still pending.

## Example: 3D Chess

A fully functional and documented 3D chess game implemented with `hagato` can be found [here](/examples/chess). It demonstrates a great amount of features provided by the `hagato` package.

In order to run the chess example, execute `cabal run`. You need the Vulkan SDK installed and tell cabal where to find it, for example by using a `cabal.project.local` file:

```
package vulkan
  extra-lib-dirs:
    C:\Software\VulkanSDK\1.3.239.0\Lib

package VulkanMemoryAllocator
  extra-include-dirs:
    C:\Software\VulkanSDK\1.3.239.0\Include
```

The 3D models of the chess pieces and the textures of the chess board fields were created by [Yanez Designs](https://sketchfab.com/3d-models/boris-spassky-vs-robert-james-fischer-game-3-6df15d938f184b87af96fbf7a793248e). The skybox of the 3D scene was borrowed from the [Learn OpenGL cubemap tutorial](https://learnopengl.com/Advanced-OpenGL/Cubemaps). The chess board and its labels are self-made.