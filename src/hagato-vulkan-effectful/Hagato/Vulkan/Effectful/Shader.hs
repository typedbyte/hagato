module Hagato.Vulkan.Effectful.Shader
  ( allocateShaderModule
  , manageShaderModule
  , allocateFromFile
  , manageFromFile
  , module Hagato.Vulkan.Shader
  ) where

-- bytestring
import Data.ByteString qualified as BS

-- effectful-core
import Effectful (Eff, (:>))

-- hagato:with-vulkan
import Hagato.Vulkan qualified as Vk
import Hagato.Vulkan.Shader

-- resource-effectful
import Effectful.Resource (Key, Resource, allocate)

allocateShaderModule :: Resource :> es => Vk.Device -> BS.ByteString -> Eff es (Vk.ShaderModule, Key)
allocateShaderModule device bytes =
  allocate
    ( Vk.createShaderModule device bytes )
    ( Vk.destroyShaderModule device )

manageShaderModule :: Resource :> es => Vk.Device -> BS.ByteString -> Eff es Vk.ShaderModule
manageShaderModule device bytes =
  fst <$> allocateShaderModule device bytes

allocateFromFile :: Resource :> es => FilePath -> Vk.Device -> Eff es (Vk.ShaderModule, Key)
allocateFromFile filePath device =
  allocate
    ( Vk.createFromFile filePath device )
    ( Vk.destroyShaderModule device )

manageFromFile :: Resource :> es => FilePath -> Vk.Device -> Eff es Vk.ShaderModule
manageFromFile filePath device =
  fst <$> allocateFromFile filePath device
