using System;
using NUnit.Framework;

namespace Digits_Recognizer.Test
{
    [TestFixture]
    public class TestClass
    {
        [Test]
        public void Test()
        {
            var x = Beginning.beginning.distance(new[] {1, 2}, new[] {1, 2});
            Assert.That(x, Is.EqualTo(0));
        } 
        
        [Test]
        public void Test2()
        {
            var x = Beginning.beginning.distance(new[] {2, 4}, new[] {1, 2});
            Assert.That(x, Is.EqualTo(Math.Sqrt(5)));
        }
    }
}
