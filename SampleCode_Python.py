## Implement a Deep Convolutional Generative Adversarial Network for generating fasion MNIST photos

import tensorflow as tf
import numpy as np
import matplotlib.pyplot as plt
%matplotlib inline
tf.enable_eager_execution()


## INPUT DATA
fashion_mnist = tf.keras.datasets.fashion_mnist
(train_images, train_labels), (test_images, test_labels) = fashion_mnist.load_data()

train_images = train_images.reshape(train_images.shape[0], 28, 28, 1).astype('float32')
train_images = (train_images - 127.5) / 127.5

##  BUILDING MODELS
def make_generator():
    model = tf.keras.Sequential()
    model.add(tf.keras.layers.Dense(7*7*128, use_bias=False, input_shape=(100,)))
    model.add(tf.keras.layers.BatchNormalization())
    model.add(tf.keras.layers.LeakyReLU())
    model.add(tf.keras.layers.Reshape((7, 7, 128)))
    model.add(tf.keras.layers.Conv2DTranspose(128, (5, 5), strides=(1, 1), padding='same', use_bias=False))
    model.add(tf.keras.layers.BatchNormalization())
    model.add(tf.keras.layers.LeakyReLU())
    model.add(tf.keras.layers.Conv2DTranspose(64, (5, 5), strides=(2, 2), padding='same', use_bias=False))
    model.add(tf.keras.layers.BatchNormalization())
    model.add(tf.keras.layers.LeakyReLU())
    model.add(tf.keras.layers.Conv2DTranspose(1, (5, 5), strides=(2, 2), padding='same', use_bias=False, activation='tanh'))

    return model

def make_discriminator():
    model = tf.keras.Sequential()
    model.add(tf.keras.layers.Conv2D(64, (5, 5), strides=(2, 2), padding='same',input_shape=[28, 28, 1]))
    model.add(tf.keras.layers.LeakyReLU())
    model.add(tf.keras.layers.Dropout(0.3))
    model.add(tf.keras.layers.Conv2D(128, (5, 5), strides=(2, 2), padding='same'))
    model.add(tf.keras.layers.LeakyReLU())
    model.add(tf.keras.layers.Dropout(0.3))
    model.add(tf.keras.layers.Flatten())
    model.add(tf.keras.layers.Dense(1))

    return model

## CREAT LOSS FUNCTION

def discriminator_loss(real_output, fake_output):
  real_loss = cross_entropy(tf.ones_like(real_output), real_output)
  fake_loss = cross_entropy(tf.zeros_like(fake_output), fake_output)
  total_loss = real_loss + fake_loss
  return total_loss

def generator_loss(fake_output):
  return cross_entropy(tf.ones_like(fake_output), fake_output)

cross_entropy = tf.keras.losses.BinaryCrossentropy(from_logits=True)
generator_optimizer = tf.keras.optimizers.Adam(5e-3)
discriminator_optimizer = tf.keras.optimizers.Adam(5e-3)

@tf.function
def train_step(train_sample, generator, discriminator):
  #noise = tf.random.uniform((train_sample.shape[0], 100), minval=-1., maxval=1.)
  noise = tf.random.normal([100, 100])
  #train_sample = tf.sort(train_sample, axis=-1, direction='ASCENDING')
  #print(train_sample.shape)
  #print("start training")

  with tf.GradientTape() as gen_tape, tf.GradientTape() as disc_tape:
    generated_samples = generator(noise, training=True)

    real_output = discriminator(train_sample, training=True)
    fake_output = discriminator(generated_samples, training=True)
    #print("output")
    #print(generated_samples.shape)
    #print(fake_output)

    gen_loss = generator_loss(fake_output)
    disc_loss = discriminator_loss(real_output, fake_output)

  gradients_of_generator = gen_tape.gradient(gen_loss, generator.trainable_variables)
  gradients_of_discriminator = disc_tape.gradient(disc_loss, discriminator.trainable_variables)

  generator_optimizer.apply_gradients(zip(gradients_of_generator, generator.trainable_variables))
  discriminator_optimizer.apply_gradients(zip(gradients_of_discriminator, discriminator.trainable_variables))

  return gen_loss, disc_loss

## TRAIN WITH CV

def train(num_epochs=1000, num_batches=100):
  generator = make_generator()
  discriminator = make_discriminator()
  gens = []
  discs = []

  for epoch in range(50):
    for start_idx in range(0, train_images.shape[0], num_batches):
          end_idx = start_idx + num_batches
          x_train_ = train_images[start_idx:end_idx, :]
          gen_loss, disc_loss = train_step(x_train_, generator, discriminator )

    gen = gen_loss.numpy().mean()
    disc = disc_loss.numpy().mean()
    gens.append(gen)
    discs.append(disc)
    print(f'On epoch {epoch} the generator loss is {gen:.2f}, the discriminator loss is {disc:.2f}')
  return gens, discs, generator, discriminator

##VISUALIZATION
plt.plot(range(len(gens)), gens, range(len(discs)), discs)
plt.legend(['Generator Loss', 'Discriminator Loss'])

#noise = tf.random.uniform((1, 784), minval=-1., maxval=1.)
tf.random.set_random_seed(3)
noise = tf.random.normal([1, 100])
x_hat_test = generator(noise)
test_img = np.reshape(x_hat_test.numpy()[0,:], (28, 28))
plt.imshow(test_img, cmap=plt.cm.gray)