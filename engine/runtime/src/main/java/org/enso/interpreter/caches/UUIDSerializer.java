package org.enso.interpreter.caches;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import java.util.UUID;

public class UUIDSerializer extends Serializer<UUID> {

  public UUIDSerializer() {
    setImmutable(true);
  }

  @Override
  public void write(final Kryo kryo, final Output output, final UUID uuid) {
    output.writeLong(uuid.getMostSignificantBits());
    output.writeLong(uuid.getLeastSignificantBits());
  }

  @Override
  public UUID read(final Kryo kryo, final Input input, final Class<? extends UUID> uuidClass) {
    return new UUID(input.readLong(), input.readLong());
  }
}
